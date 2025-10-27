"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.CliIoHost = void 0;
const util = require("node:util");
const cloud_assembly_schema_1 = require("@aws-cdk/cloud-assembly-schema");
const toolkit_lib_1 = require("@aws-cdk/toolkit-lib");
const chalk = require("chalk");
const promptly = require("promptly");
const api_private_1 = require("../../../lib/api-private");
const deploy_1 = require("../../commands/deploy");
const messages_1 = require("../telemetry/messages");
const session_1 = require("../telemetry/session");
const file_sink_1 = require("../telemetry/sink/file-sink");
const ci_1 = require("../util/ci");
/**
 * A simple IO host for the CLI that writes messages to the console.
 */
class CliIoHost {
    /**
     * Returns the singleton instance
     */
    static instance(props = {}, forceNew = false) {
        if (forceNew || !CliIoHost._instance) {
            CliIoHost._instance = new CliIoHost(props);
        }
        return CliIoHost._instance;
    }
    /**
     * Returns the singleton instance if it exists
     */
    static get() {
        return CliIoHost._instance;
    }
    constructor(props = {}) {
        /**
         * Configure the target stream for notices
         *
         * (Not a setter because there's no need for additional logic when this value
         * is changed yet)
         */
        this.noticesDestination = 'stderr';
        this._progress = deploy_1.StackActivityProgress.BAR;
        // Corked Logging
        this.corkedCounter = 0;
        this.corkedLoggingBuffer = [];
        this.currentAction = props.currentAction ?? 'none';
        this.isTTY = props.isTTY ?? process.stdout.isTTY ?? false;
        this.logLevel = props.logLevel ?? 'info';
        this.isCI = props.isCI ?? (0, ci_1.isCI)();
        this.requireDeployApproval = props.requireDeployApproval ?? cloud_assembly_schema_1.RequireApproval.BROADENING;
        this.stackProgress = props.stackProgress ?? deploy_1.StackActivityProgress.BAR;
    }
    async startTelemetry(args, context, _proxyAgent) {
        let sink;
        const telemetryFilePath = args['telemetry-file'];
        if (telemetryFilePath) {
            sink = new file_sink_1.FileTelemetrySink({
                ioHost: this,
                logFilePath: telemetryFilePath,
            });
        }
        // TODO: uncomment this at launch
        // if (canCollectTelemetry(args, context)) {
        //   sink = new EndpointTelemetrySink({
        //     ioHost: this,
        //     agent: proxyAgent,
        //     endpoint: '', // TODO: add endpoint
        //   });
        // }
        if (sink) {
            this.telemetry = new session_1.TelemetrySession({
                ioHost: this,
                client: sink,
                arguments: args,
                context: context,
            });
        }
        await this.telemetry?.begin();
    }
    /**
     * Update the stackProgress preference.
     */
    set stackProgress(type) {
        this._progress = type;
    }
    /**
     * Gets the stackProgress value.
     *
     * This takes into account other state of the ioHost,
     * like if isTTY and isCI.
     */
    get stackProgress() {
        // We can always use EVENTS
        if (this._progress === deploy_1.StackActivityProgress.EVENTS) {
            return this._progress;
        }
        // if a debug message (and thus any more verbose messages) are relevant to the current log level, we have verbose logging
        const verboseLogging = (0, api_private_1.isMessageRelevantForLevel)({ level: 'debug' }, this.logLevel);
        if (verboseLogging) {
            return deploy_1.StackActivityProgress.EVENTS;
        }
        // On Windows we cannot use fancy output
        const isWindows = process.platform === 'win32';
        if (isWindows) {
            return deploy_1.StackActivityProgress.EVENTS;
        }
        // On some CI systems (such as CircleCI) output still reports as a TTY so we also
        // need an individual check for whether we're running on CI.
        // see: https://discuss.circleci.com/t/circleci-terminal-is-a-tty-but-term-is-not-set/9965
        const fancyOutputAvailable = this.isTTY && !this.isCI;
        if (!fancyOutputAvailable) {
            return deploy_1.StackActivityProgress.EVENTS;
        }
        // Use the user preference
        return this._progress;
    }
    get defaults() {
        return this.asIoHelper().defaults;
    }
    asIoHelper() {
        return (0, api_private_1.asIoHelper)(this, this.currentAction);
    }
    /**
     * Executes a block of code with corked logging. All log messages during execution
     * are buffered and only written when all nested cork blocks complete (when CORK_COUNTER reaches 0).
     * The corking is bound to the specific instance of the CliIoHost.
     *
     * @param block - Async function to execute with corked logging
     * @returns Promise that resolves with the block's return value
     */
    async withCorkedLogging(block) {
        this.corkedCounter++;
        try {
            return await block();
        }
        finally {
            this.corkedCounter--;
            if (this.corkedCounter === 0) {
                // Process each buffered message through notify
                for (const ioMessage of this.corkedLoggingBuffer) {
                    await this.notify(ioMessage);
                }
                // remove all buffered messages in-place
                this.corkedLoggingBuffer.splice(0);
            }
        }
    }
    /**
     * Notifies the host of a message.
     * The caller waits until the notification completes.
     */
    async notify(msg) {
        await this.maybeEmitTelemetry(msg);
        if (this.isStackActivity(msg)) {
            if (!this.activityPrinter) {
                this.activityPrinter = this.makeActivityPrinter();
            }
            this.activityPrinter.notify(msg);
            return;
        }
        if (!(0, api_private_1.isMessageRelevantForLevel)(msg, this.logLevel)) {
            return;
        }
        if (this.corkedCounter > 0) {
            this.corkedLoggingBuffer.push(msg);
            return;
        }
        const output = this.formatMessage(msg);
        const stream = this.selectStream(msg);
        stream?.write(output);
    }
    async maybeEmitTelemetry(msg) {
        try {
            if (this.telemetry && isTelemetryMessage(msg)) {
                await this.telemetry.emit({
                    eventType: getEventType(msg),
                    duration: msg.data.duration,
                    error: msg.data.error,
                });
            }
        }
        catch (e) {
            await this.defaults.trace(`Emit Telemetry Failed ${e.message}`);
        }
    }
    /**
     * Detect stack activity messages so they can be send to the printer.
     */
    isStackActivity(msg) {
        return msg.code && [
            'CDK_TOOLKIT_I5501',
            'CDK_TOOLKIT_I5502',
            'CDK_TOOLKIT_I5503',
        ].includes(msg.code);
    }
    /**
     * Detect special messages encode information about whether or not
     * they require approval
     */
    skipApprovalStep(msg) {
        const approvalToolkitCodes = ['CDK_TOOLKIT_I5060'];
        if (!(msg.code && approvalToolkitCodes.includes(msg.code))) {
            false;
        }
        switch (this.requireDeployApproval) {
            // Never require approval
            case cloud_assembly_schema_1.RequireApproval.NEVER:
                return true;
            // Always require approval
            case cloud_assembly_schema_1.RequireApproval.ANYCHANGE:
                return false;
            // Require approval if changes include broadening permissions
            case cloud_assembly_schema_1.RequireApproval.BROADENING:
                return ['none', 'non-broadening'].includes(msg.data?.permissionChangeType);
        }
    }
    /**
     * Determines the output stream, based on message and configuration.
     */
    selectStream(msg) {
        if (isNoticesMessage(msg)) {
            return targetStreamObject(this.noticesDestination);
        }
        return this.selectStreamFromLevel(msg.level);
    }
    /**
     * Determines the output stream, based on message level and configuration.
     */
    selectStreamFromLevel(level) {
        // The stream selection policy for the CLI is the following:
        //
        //   (1) Messages of level `result` always go to `stdout`
        //   (2) Messages of level `error` always go to `stderr`.
        //   (3a) All remaining messages go to `stderr`.
        //   (3b) If we are in CI mode, all remaining messages go to `stdout`.
        //
        switch (level) {
            case 'error':
                return process.stderr;
            case 'result':
                return process.stdout;
            default:
                return this.isCI ? process.stdout : process.stderr;
        }
    }
    /**
     * Notifies the host of a message that requires a response.
     *
     * If the host does not return a response the suggested
     * default response from the input message will be used.
     */
    async requestResponse(msg) {
        // If the request cannot be prompted for by the CliIoHost, we just accept the default
        if (!isPromptableRequest(msg)) {
            await this.notify(msg);
            return msg.defaultResponse;
        }
        const response = await this.withCorkedLogging(async () => {
            // prepare prompt data
            // @todo this format is not defined anywhere, probably should be
            const data = msg.data ?? {};
            const motivation = data.motivation ?? 'User input is needed';
            const concurrency = data.concurrency ?? 0;
            const responseDescription = data.responseDescription;
            // only talk to user if STDIN is a terminal (otherwise, fail)
            if (!this.isTTY) {
                throw new toolkit_lib_1.ToolkitError(`${motivation}, but terminal (TTY) is not attached so we are unable to get a confirmation from the user`);
            }
            // only talk to user if concurrency is 1 (otherwise, fail)
            if (concurrency > 1) {
                throw new toolkit_lib_1.ToolkitError(`${motivation}, but concurrency is greater than 1 so we are unable to get a confirmation from the user`);
            }
            // Special approval prompt
            // Determine if the message needs approval. If it does, continue (it is a basic confirmation prompt)
            // If it does not, return success (true). We only check messages with codes that we are aware
            // are requires approval codes.
            if (this.skipApprovalStep(msg)) {
                return true;
            }
            // Basic confirmation prompt
            // We treat all requests with a boolean response as confirmation prompts
            if (isConfirmationPrompt(msg)) {
                const confirmed = await promptly.confirm(`${chalk.cyan(msg.message)} (y/n)`);
                if (!confirmed) {
                    throw new toolkit_lib_1.ToolkitError('Aborted by user');
                }
                return confirmed;
            }
            // Asking for a specific value
            const prompt = extractPromptInfo(msg);
            const desc = responseDescription ?? prompt.default;
            const answer = await promptly.prompt(`${chalk.cyan(msg.message)}${desc ? ` (${desc})` : ''}`, {
                default: prompt.default,
                trim: true,
            });
            return prompt.convertAnswer(answer);
        });
        // We need to cast this because it is impossible to narrow the generic type
        // isPromptableRequest ensures that the response type is one we can prompt for
        // the remaining code ensure we are indeed returning the correct type
        return response;
    }
    /**
     * Formats a message for console output with optional color support
     */
    formatMessage(msg) {
        // apply provided style or a default style if we're in TTY mode
        let message_text = this.isTTY
            ? styleMap[msg.level](msg.message)
            : msg.message;
        // prepend timestamp if IoMessageLevel is DEBUG or TRACE. Postpend a newline.
        return ((msg.level === 'debug' || msg.level === 'trace')
            ? `[${this.formatTime(msg.time)}] ${message_text}`
            : message_text) + '\n';
    }
    /**
     * Formats date to HH:MM:SS
     */
    formatTime(d) {
        const pad = (n) => n.toString().padStart(2, '0');
        return `${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
    }
    /**
     * Get an instance of the ActivityPrinter
     */
    makeActivityPrinter() {
        const props = {
            stream: this.selectStreamFromLevel('info'),
        };
        switch (this.stackProgress) {
            case deploy_1.StackActivityProgress.EVENTS:
                return new api_private_1.HistoryActivityPrinter(props);
            case deploy_1.StackActivityProgress.BAR:
                return new api_private_1.CurrentActivityPrinter(props);
        }
    }
}
exports.CliIoHost = CliIoHost;
/**
 * This IoHost implementation considers a request promptable, if:
 * - it's a yes/no confirmation
 * - asking for a string or number value
 */
function isPromptableRequest(msg) {
    return isConfirmationPrompt(msg)
        || typeof msg.defaultResponse === 'string'
        || typeof msg.defaultResponse === 'number';
}
/**
 * Check if the request is a confirmation prompt
 * We treat all requests with a boolean response as confirmation prompts
 */
function isConfirmationPrompt(msg) {
    return typeof msg.defaultResponse === 'boolean';
}
/**
 * Helper to extract information for promptly from the request
 */
function extractPromptInfo(msg) {
    const isNumber = (typeof msg.defaultResponse === 'number');
    const defaultResponse = util.format(msg.defaultResponse);
    return {
        default: defaultResponse,
        defaultDesc: 'defaultDescription' in msg && msg.defaultDescription ? util.format(msg.defaultDescription) : defaultResponse,
        convertAnswer: isNumber ? (v) => Number(v) : (v) => String(v),
    };
}
const styleMap = {
    error: chalk.red,
    warn: chalk.yellow,
    result: chalk.reset,
    info: chalk.reset,
    debug: chalk.gray,
    trace: chalk.gray,
};
function targetStreamObject(x) {
    switch (x) {
        case 'stderr':
            return process.stderr;
        case 'stdout':
            return process.stdout;
        case 'drop':
            return undefined;
    }
}
function isNoticesMessage(msg) {
    return api_private_1.IO.CDK_TOOLKIT_I0100.is(msg) || api_private_1.IO.CDK_TOOLKIT_W0101.is(msg) || api_private_1.IO.CDK_TOOLKIT_E0101.is(msg) || api_private_1.IO.CDK_TOOLKIT_I0101.is(msg);
}
function isTelemetryMessage(msg) {
    return messages_1.CLI_TELEMETRY_CODES.some((c) => c.is(msg));
}
function getEventType(msg) {
    switch (msg.code) {
        case messages_1.CLI_PRIVATE_IO.CDK_CLI_I1001.code:
            return 'SYNTH';
        case messages_1.CLI_PRIVATE_IO.CDK_CLI_I2001.code:
            return 'INVOKE';
        case messages_1.CLI_PRIVATE_IO.CDK_CLI_I3001.code:
            return 'DEPLOY';
        default:
            throw new toolkit_lib_1.ToolkitError(`Unrecognized Telemetry Message Code: ${msg.code}`);
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2xpLWlvLWhvc3QuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyJjbGktaW8taG9zdC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7QUFDQSxrQ0FBa0M7QUFDbEMsMEVBQWlFO0FBQ2pFLHNEQUFvRDtBQUdwRCwrQkFBK0I7QUFDL0IscUNBQXFDO0FBRXJDLDBEQUFxSTtBQUNySSxrREFBOEQ7QUFFOUQsb0RBQTRFO0FBRTVFLGtEQUF3RDtBQUN4RCwyREFBZ0U7QUFDaEUsbUNBQWtDO0FBMEVsQzs7R0FFRztBQUNILE1BQWEsU0FBUztJQUNwQjs7T0FFRztJQUNILE1BQU0sQ0FBQyxRQUFRLENBQUMsUUFBd0IsRUFBRSxFQUFFLFFBQVEsR0FBRyxLQUFLO1FBQzFELElBQUksUUFBUSxJQUFJLENBQUMsU0FBUyxDQUFDLFNBQVMsRUFBRSxDQUFDO1lBQ3JDLFNBQVMsQ0FBQyxTQUFTLEdBQUcsSUFBSSxTQUFTLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDN0MsQ0FBQztRQUNELE9BQU8sU0FBUyxDQUFDLFNBQVMsQ0FBQztJQUM3QixDQUFDO0lBRUQ7O09BRUc7SUFDSCxNQUFNLENBQUMsR0FBRztRQUNSLE9BQU8sU0FBUyxDQUFDLFNBQVMsQ0FBQztJQUM3QixDQUFDO0lBdURELFlBQW9CLFFBQXdCLEVBQUU7UUFuQjlDOzs7OztXQUtHO1FBQ0ksdUJBQWtCLEdBQWlCLFFBQVEsQ0FBQztRQUUzQyxjQUFTLEdBQTBCLDhCQUFxQixDQUFDLEdBQUcsQ0FBQztRQUtyRSxpQkFBaUI7UUFDVCxrQkFBYSxHQUFHLENBQUMsQ0FBQztRQUNULHdCQUFtQixHQUF5QixFQUFFLENBQUM7UUFLOUQsSUFBSSxDQUFDLGFBQWEsR0FBRyxLQUFLLENBQUMsYUFBYSxJQUFJLE1BQU0sQ0FBQztRQUNuRCxJQUFJLENBQUMsS0FBSyxHQUFHLEtBQUssQ0FBQyxLQUFLLElBQUksT0FBTyxDQUFDLE1BQU0sQ0FBQyxLQUFLLElBQUksS0FBSyxDQUFDO1FBQzFELElB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SUFBSTtZQUNwQyxPQUFPLFFBQVEsQ0FBQztRQUNsQjtZQUNFLE1BQU0sSUFBSSwwQkFBWSxDQUFDLHdDQUF3QyxHQUFHLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQztJQUMvRSxDQUFDO0FBQ0gsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB0eXBlIHsgQWdlbnQgfSBmcm9tICdub2RlOmh0dHBzJztcbmltcG9ydCAqIGFzIHV0aWwgZnJvbSAnbm9kZTp1dGlsJztcbmltcG9ydCB7IFJlcXVpcmVBcHByb3ZhbCB9IGZyb20gJ0Bhd3MtY2RrL2Nsb3VkLWFzc2VtYmx5LXNjaGVtYSc7XG5pbXBvcnQgeyBUb29sa2l0RXJyb3IgfSBmcm9tICdAYXdzLWNkay90b29sa2l0LWxpYic7XG5pbXBvcnQgdHlwZSB7IElJb0hvc3QsIElvTWVzc2FnZSwgSW9NZXNzYWdlQ29kZSwgSW9NZXNzYWdlTGV2ZWwsIElvUmVxdWVzdCwgVG9vbGtpdEFjdGlvbiB9IGZyb20gJ0Bhd3MtY2RrL3Rvb2xraXQtbGliJztcbmltcG9ydCB0eXBlIHsgQ29udGV4dCB9IGZyb20gJ0Bhd3MtY2RrL3Rvb2xraXQtbGliL2xpYi9hcGknO1xuaW1wb3J0ICogYXMgY2hhbGsgZnJvbSAnY2hhbGsnO1xuaW1wb3J0ICogYXMgcHJvbXB0bHkgZnJvbSAncHJvbXB0bHknO1xuaW1wb3J0IHR5cGUgeyBJb0hlbHBlciwgQWN0aXZpdHlQcmludGVyUHJvcHMsIElBY3Rpdml0eVByaW50ZXIgfSBmcm9tICcuLi8uLi8uLi9saWIvYXBpLXByaXZhdGUnO1xuaW1wb3J0IHsgYXNJb0hlbHBlciwgSU8sIGlzTWVzc2FnZVJlbGV2YW50Rm9yTGV2ZWwsIEN1cnJlbnRBY3Rpdml0eVByaW50ZXIsIEhpc3RvcnlBY3Rpdml0eVByaW50ZXIgfSBmcm9tICcuLi8uLi8uLi9saWIvYXBpLXByaXZhdGUnO1xuaW1wb3J0IHsgU3RhY2tBY3Rpdml0eVByb2dyZXNzIH0gZnJvbSAnLi4vLi4vY29tbWFuZHMvZGVwbG95JztcbmltcG9ydCB0eXBlIHsgRXZlbnRSZXN1bHQgfSBmcm9tICcuLi90ZWxlbWV0cnkvbWVzc2FnZXMnO1xuaW1wb3J0IHsgQ0xJX1BSSVZBVEVfSU8sIENMSV9URUxFTUVUUllfQ09ERVMgfSBmcm9tICcuLi90ZWxlbWV0cnkvbWVzc2FnZXMnO1xuaW1wb3J0IHR5cGUgeyBFdmVudFR5cGUgfSBmcm9tICcuLi90ZWxlbWV0cnkvc2NoZW1hJztcbmltcG9ydCB7IFRlbGVtZXRyeVNlc3Npb24gfSBmcm9tICcuLi90ZWxlbWV0cnkvc2Vzc2lvbic7XG5pbXBvcnQgeyBGaWxlVGVsZW1ldHJ5U2luayB9IGZyb20gJy4uL3RlbGVtZXRyeS9zaW5rL2ZpbGUtc2luayc7XG5pbXBvcnQgeyBpc0NJIH0gZnJvbSAnLi4vdXRpbC9jaSc7XG5cbmV4cG9ydCB0eXBlIHsgSUlvSG9zdCwgSW9NZXNzYWdlLCBJb01lc3NhZ2VDb2RlLCBJb01lc3NhZ2VMZXZlbCwgSW9SZXF1ZXN0IH07XG5cbi8qKlxuICogVGhlIGN1cnJlbnQgYWN0aW9uIGJlaW5nIHBlcmZvcm1lZCBieSB0aGUgQ0xJLiAnbm9uZScgcmVwcmVzZW50cyB0aGUgYWJzZW5jZSBvZiBhbiBhY3Rpb24uXG4gKi9cbnR5cGUgQ2xpQWN0aW9uID1cbnwgVG9vbGtpdEFjdGlvblxufCAnY29udGV4dCdcbnwgJ2RvY3MnXG58ICdmbGFncydcbnwgJ25vdGljZXMnXG58ICd2ZXJzaW9uJ1xufCAnY2xpLXRlbGVtZXRyeSdcbnwgJ25vbmUnO1xuXG5leHBvcnQgaW50ZXJmYWNlIENsaUlvSG9zdFByb3BzIHtcbiAgLyoqXG4gICAqIFRoZSBpbml0aWFsIFRvb2xraXQgYWN0aW9uIHRoZSBob3N0cyBzdGFydHMgd2l0aC5cbiAgICpcbiAgICogQGRlZmF1bHQgJ25vbmUnXG4gICAqL1xuICByZWFkb25seSBjdXJyZW50QWN0aW9uPzogQ2xpQWN0aW9uO1xuXG4gIC8qKlxuICAgKiBEZXRlcm1pbmVzIHRoZSB2ZXJib3NpdHkgb2YgdGhlIG91dHB1dC5cbiAgICpcbiAgICogVGhlIENsaUlvSG9zdCB3aWxsIHN0aWxsIHJlY2VpdmUgYWxsIG1lc3NhZ2VzIGFuZCByZXF1ZXN0cyxcbiAgICogYnV0IG9ubHkgdGhlIG1lc3NhZ2VzIGluY2x1ZGVkIGluIHRoaXMgbGV2ZWwgd2lsbCBiZSBwcmludGVkLlxuICAgKlxuICAgKiBAZGVmYXVsdCAnaW5mbydcbiAgICovXG4gIHJlYWRvbmx5IGxvZ0xldmVsPzogSW9NZXNzYWdlTGV2ZWw7XG5cbiAgLyoqXG4gICAqIE92ZXJyaWRlcyB0aGUgYXV0b21hdGljIFRUWSBkZXRlY3Rpb24uXG4gICAqXG4gICAqIFdoZW4gVFRZIGlzIGRpc2FibGVkLCB0aGUgQ0xJIHdpbGwgaGF2ZSBubyBpbnRlcmFjdGlvbnMgb3IgY29sb3IuXG4gICAqXG4gICAqIEBkZWZhdWx0IC0gZGV0ZXJtaW5lZCBmcm9tIHRoZSBjdXJyZW50IHByb2Nlc3NcbiAgICovXG4gIHJlYWRvbmx5IGlzVFRZPzogYm9vbGVhbjtcblxuICAvKipcbiAgICogV2hldGhlciB0aGUgQ2xpSW9Ib3N0IGlzIHJ1bm5pbmcgaW4gQ0kgbW9kZS5cbiAgICpcbiAgICogSW4gQ0kgbW9kZSwgYWxsIG5vbi1lcnJvciBvdXRwdXQgZ29lcyB0byBzdGRvdXQgaW5zdGVhZCBvZiBzdGRlcnIuXG4gICAqIFNldCB0byBmYWxzZSBpbiB0aGUgQ2xpSW9Ib3N0IGNvbnN0cnVjdG9yIGl0IHdpbGwgYmUgb3ZlcndyaXR0ZW4gaWYgdGhlIENMSSBDSSBhcmd1bWVudCBpcyBwYXNzZWRcbiAgICpcbiAgICogQGRlZmF1bHQgLSBkZXRlcm1pbmVkIGZyb20gdGhlIGVudmlyb25tZW50LCBzcGVjaWZpY2FsbHkgYmFzZWQgb24gYHByb2Nlc3MuZW52LkNJYFxuICAgKi9cbiAgcmVhZG9ubHkgaXNDST86IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIEluIHdoYXQgc2NlbmFyaW9zIHNob3VsZCB0aGUgQ2xpSW9Ib3N0IGFzayBmb3IgYXBwcm92YWxcbiAgICpcbiAgICogQGRlZmF1bHQgUmVxdWlyZUFwcHJvdmFsLkJST0FERU5JTkdcbiAgICovXG4gIHJlYWRvbmx5IHJlcXVpcmVEZXBsb3lBcHByb3ZhbD86IFJlcXVpcmVBcHByb3ZhbDtcblxuICAvKipcbiAgICogVGhlIGluaXRpYWwgVG9vbGtpdCBhY3Rpb24gdGhlIGhvc3RzIHN0YXJ0cyB3aXRoLlxuICAgKlxuICAgKiBAZGVmYXVsdCBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MuQkFSXG4gICAqL1xuICByZWFkb25seSBzdGFja1Byb2dyZXNzPzogU3RhY2tBY3Rpdml0eVByb2dyZXNzO1xufVxuXG4vKipcbiAqIEEgdHlwZSBmb3IgY29uZmlndXJpbmcgYSB0YXJnZXQgc3RyZWFtXG4gKi9cbmV4cG9ydCB0eXBlIFRhcmdldFN0cmVhbSA9ICdzdGRvdXQnIHwgJ3N0ZGVycicgfCAnZHJvcCc7XG5cbi8qKlxuICogQSBzaW1wbGUgSU8gaG9zdCBmb3IgdGhlIENMSSB0aGF0IHdyaXRlcyBtZXNzYWdlcyB0byB0aGUgY29uc29sZS5cbiAqL1xuZXhwb3J0IGNsYXNzIENsaUlvSG9zdCBpbXBsZW1lbnRzIElJb0hvc3Qge1xuICAvKipcbiAgICogUmV0dXJucyB0aGUgc2luZ2xldG9uIGluc3RhbmNlXG4gICAqL1xuICBzdGF0aWMgaW5zdGFuY2UocHJvcHM6IENsaUlvSG9zdFByb3BzID0ge30sIGZvcmNlTmV3ID0gZmFsc2UpOiBDbGlJb0hvc3Qge1xuICAgIGlmIChmb3JjZU5ldyB8fCAhQ2xpSW9Ib3N0Ll9pbnN0YW5jZSkge1xuICAgICAgQ2xpSW9Ib3N0Ll9pbnN0YW5jZSA9IG5ldyBDbGlJb0hvc3QocHJvcHMpO1xuICAgIH1cbiAgICByZXR1cm4gQ2xpSW9Ib3N0Ll9pbnN0YW5jZTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIHRoZSBzaW5nbGV0b24gaW5zdGFuY2UgaWYgaXQgZXhpc3RzXG4gICAqL1xuICBzdGF0aWMgZ2V0KCk6IENsaUlvSG9zdCB8IHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIENsaUlvSG9zdC5faW5zdGFuY2U7XG4gIH1cblxuICAvKipcbiAgICogU2luZ2xldG9uIGluc3RhbmNlIG9mIHRoZSBDbGlJb0hvc3RcbiAgICovXG4gIHByaXZhdGUgc3RhdGljIF9pbnN0YW5jZTogQ2xpSW9Ib3N0IHwgdW5kZWZpbmVkO1xuXG4gIC8qKlxuICAgKiBUaGUgY3VycmVudCBhY3Rpb24gYmVpbmcgcGVyZm9ybWVkIGJ5IHRoZSBDTEkuXG4gICAqL1xuICBwdWJsaWMgY3VycmVudEFjdGlvbjogQ2xpQWN0aW9uO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBDbGlJb0hvc3QgaXMgcnVubmluZyBpbiBDSSBtb2RlLlxuICAgKlxuICAgKiBJbiBDSSBtb2RlLCBhbGwgbm9uLWVycm9yIG91dHB1dCBnb2VzIHRvIHN0ZG91dCBpbnN0ZWFkIG9mIHN0ZGVyci5cbiAgICovXG4gIHB1YmxpYyBpc0NJOiBib29sZWFuO1xuXG4gIC8qKlxuICAgKiBXaGV0aGVyIHRoZSBob3N0IGNhbiB1c2UgaW50ZXJhY3Rpb25zIGFuZCBtZXNzYWdlIHN0eWxpbmcuXG4gICAqL1xuICBwdWJsaWMgaXNUVFk6IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIFRoZSBjdXJyZW50IHRocmVzaG9sZC5cbiAgICpcbiAgICogTWVzc2FnZXMgd2l0aCBhIGxvd2VyIHByaW9yaXR5IGxldmVsIHdpbGwgYmUgaWdub3JlZC5cbiAgICovXG4gIHB1YmxpYyBsb2dMZXZlbDogSW9NZXNzYWdlTGV2ZWw7XG5cbiAgLyoqXG4gICAqIFRoZSBjb25kaXRpb25zIGZvciByZXF1aXJpbmcgYXBwcm92YWwgaW4gdGhpcyBDbGlJb0hvc3QuXG4gICAqL1xuICBwdWJsaWMgcmVxdWlyZURlcGxveUFwcHJvdmFsOiBSZXF1aXJlQXBwcm92YWw7XG5cbiAgLyoqXG4gICAqIENvbmZpZ3VyZSB0aGUgdGFyZ2V0IHN0cmVhbSBmb3Igbm90aWNlc1xuICAgKlxuICAgKiAoTm90IGEgc2V0dGVyIGJlY2F1c2UgdGhlcmUncyBubyBuZWVkIGZvciBhZGRpdGlvbmFsIGxvZ2ljIHdoZW4gdGhpcyB2YWx1ZVxuICAgKiBpcyBjaGFuZ2VkIHlldClcbiAgICovXG4gIHB1YmxpYyBub3RpY2VzRGVzdGluYXRpb246IFRhcmdldFN0cmVhbSA9ICdzdGRlcnInO1xuXG4gIHByaXZhdGUgX3Byb2dyZXNzOiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MgPSBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MuQkFSO1xuXG4gIC8vIFN0YWNrIEFjdGl2aXR5IFByaW50ZXJcbiAgcHJpdmF0ZSBhY3Rpdml0eVByaW50ZXI/OiBJQWN0aXZpdHlQcmludGVyO1xuXG4gIC8vIENvcmtlZCBMb2dnaW5nXG4gIHByaXZhdGUgY29ya2VkQ291bnRlciA9IDA7XG4gIHByaXZhdGUgcmVhZG9ubHkgY29ya2VkTG9nZ2luZ0J1ZmZlcjogSW9NZXNzYWdlPHVua25vd24+W10gPSBbXTtcblxuICBwdWJsaWMgdGVsZW1ldHJ5PzogVGVsZW1ldHJ5U2Vzc2lvbjtcblxuICBwcml2YXRlIGNvbnN0cnVjdG9yKHByb3BzOiBDbGlJb0hvc3RQcm9wcyA9IHt9KSB7XG4gICAgdGhpcy5jdXJyZW50QWN0aW9uID0gcHJvcHMuY3VycmVudEFjdGlvbiA/PyAnbm9uZSc7XG4gICAgdGhpcy5pc1RUWSA9IHByb3BzLmlzVFRZID8/IHByb2Nlc3Muc3Rkb3V0LmlzVFRZID8/IGZhbHNlO1xuICAgIHRoaXMubG9nTGV2ZWwgPSBwcm9wcy5sb2dMZXZlbCA/PyAnaW5mbyc7XG4gICAgdGhpcy5pc0NJID0gcHJvcHMuaXNDSSA/PyBpc0NJKCk7XG4gICAgdGhpcy5yZXF1aXJlRGVwbG95QXBwcm92YWwgPSBwcm9wcy5yZXF1aXJlRGVwbG95QXBwcm92YWwgPz8gUmVxdWlyZUFwcHJvdmFsLkJST0FERU5JTkc7XG5cbiAgICB0aGlzLnN0YWNrUHJvZ3Jlc3MgPSBwcm9wcy5zdGFja1Byb2dyZXNzID8/IFN0YWNrQWN0aXZpdHlQcm9ncmVzcy5CQVI7XG4gIH1cblxuICBwdWJsaWMgYXN5bmMgc3RhcnRUZWxlbWV0cnkoYXJnczogYW55LCBjb250ZXh0OiBDb250ZXh0LCBfcHJveHlBZ2VudD86IEFnZW50KSB7XG4gICAgbGV0IHNpbms7XG4gICAgY29uc3QgdGVsZW1ldHJ5RmlsZVBhdGggPSBhcmdzWyd0ZWxlbWV0cnktZmlsZSddO1xuICAgIGlmICh0ZWxlbWV0cnlGaWxlUGF0aCkge1xuICAgICAgc2luayA9IG5ldyBGaWxlVGVsZW1ldHJ5U2luayh7XG4gICAgICAgIGlvSG9zdDogdGhpcyxcbiAgICAgICAgbG9nRmlsZVBhdGg6IHRlbGVtZXRyeUZpbGVQYXRoLFxuICAgICAgfSk7XG4gICAgfVxuICAgIC8vIFRPRE86IHVuY29tbWVudCB0aGlzIGF0IGxhdW5jaFxuICAgIC8vIGlmIChjYW5Db2xsZWN0VGVsZW1ldHJ5KGFyZ3MsIGNvbnRleHQpKSB7XG4gICAgLy8gICBzaW5rID0gbmV3IEVuZHBvaW50VGVsZW1ldHJ5U2luayh7XG4gICAgLy8gICAgIGlvSG9zdDogdGhpcyxcbiAgICAvLyAgICAgYWdlbnQ6IHByb3h5QWdlbnQsXG4gICAgLy8gICAgIGVuZHBvaW50OiAnJywgLy8gVE9ETzogYWRkIGVuZHBvaW50XG4gICAgLy8gICB9KTtcbiAgICAvLyB9XG5cbiAgICBpZiAoc2luaykge1xuICAgICAgdGhpcy50ZWxlbWV0cnkgPSBuZXcgVGVsZW1ldHJ5U2Vzc2lvbih7XG4gICAgICAgIGlvSG9zdDogdGhpcyxcbiAgICAgICAgY2xpZW50OiBzaW5rLFxuICAgICAgICBhcmd1bWVudHM6IGFyZ3MsXG4gICAgICAgIGNvbnRleHQ6IGNvbnRleHQsXG4gICAgICB9KTtcbiAgICB9XG5cbiAgICBhd2FpdCB0aGlzLnRlbGVtZXRyeT8uYmVnaW4oKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBVcGRhdGUgdGhlIHN0YWNrUHJvZ3Jlc3MgcHJlZmVyZW5jZS5cbiAgICovXG4gIHB1YmxpYyBzZXQgc3RhY2tQcm9ncmVzcyh0eXBlOiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MpIHtcbiAgICB0aGlzLl9wcm9ncmVzcyA9IHR5cGU7XG4gIH1cblxuICAvKipcbiAgICogR2V0cyB0aGUgc3RhY2tQcm9ncmVzcyB2YWx1ZS5cbiAgICpcbiAgICogVGhpcyB0YWtlcyBpbnRvIGFjY291bnQgb3RoZXIgc3RhdGUgb2YgdGhlIGlvSG9zdCxcbiAgICogbGlrZSBpZiBpc1RUWSBhbmQgaXNDSS5cbiAgICovXG4gIHB1YmxpYyBnZXQgc3RhY2tQcm9ncmVzcygpOiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3Mge1xuICAgIC8vIFdlIGNhbiBhbHdheXMgdXNlIEVWRU5UU1xuICAgIGlmICh0aGlzLl9wcm9ncmVzcyA9PT0gU3RhY2tBY3Rpdml0eVByb2dyZXNzLkVWRU5UUykge1xuICAgICAgcmV0dXJuIHRoaXMuX3Byb2dyZXNzO1xuICAgIH1cblxuICAgIC8vIGlmIGEgZGVidWcgbWVzc2FnZSAoYW5kIHRodXMgYW55IG1vcmUgdmVyYm9zZSBtZXNzYWdlcykgYXJlIHJlbGV2YW50IHRvIHRoZSBjdXJyZW50IGxvZyBsZXZlbCwgd2UgaGF2ZSB2ZXJib3NlIGxvZ2dpbmdcbiAgICBjb25zdCB2ZXJib3NlTG9nZ2luZyA9IGlzTWVzc2FnZVJlbGV2YW50Rm9yTGV2ZWwoeyBsZXZlbDogJ2RlYnVnJyB9LCB0aGlzLmxvZ0xldmVsKTtcbiAgICBpZiAodmVyYm9zZUxvZ2dpbmcpIHtcbiAgICAgIHJldHVybiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MuRVZFTlRTO1xuICAgIH1cblxuICAgIC8vIE9uIFdpbmRvd3Mgd2UgY2Fubm90IHVzZSBmYW5jeSBvdXRwdXRcbiAgICBjb25zdCBpc1dpbmRvd3MgPSBwcm9jZXNzLnBsYXRmb3JtID09PSAnd2luMzInO1xuICAgIGlmIChpc1dpbmRvd3MpIHtcbiAgICAgIHJldHVybiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MuRVZFTlRTO1xuICAgIH1cblxuICAgIC8vIE9uIHNvbWUgQ0kgc3lzdGVtcyAoc3VjaCBhcyBDaXJjbGVDSSkgb3V0cHV0IHN0aWxsIHJlcG9ydHMgYXMgYSBUVFkgc28gd2UgYWxzb1xuICAgIC8vIG5lZWQgYW4gaW5kaXZpZHVhbCBjaGVjayBmb3Igd2hldGhlciB3ZSdyZSBydW5uaW5nIG9uIENJLlxuICAgIC8vIHNlZTogaHR0cHM6Ly9kaXNjdXNzLmNpcmNsZWNpLmNvbS90L2NpcmNsZWNpLXRlcm1pbmFsLWlzLWEtdHR5LWJ1dC10ZXJtLWlzLW5vdC1zZXQvOTk2NVxuICAgIGNvbnN0IGZhbmN5T3V0cHV0QXZhaWxhYmxlID0gdGhpcy5pc1RUWSAmJiAhdGhpcy5pc0NJO1xuICAgIGlmICghZmFuY3lPdXRwdXRBdmFpbGFibGUpIHtcbiAgICAgIHJldHVybiBTdGFja0FjdGl2aXR5UHJvZ3Jlc3MuRVZFTlRTO1xuICAgIH1cblxuICAgIC8vIFVzZSB0aGUgdXNlciBwcmVmZXJlbmNlXG4gICAgcmV0dXJuIHRoaXMuX3Byb2dyZXNzO1xuICB9XG5cbiAgcHVibGljIGdldCBkZWZhdWx0cygpIHtcbiAgICByZXR1cm4gdGhpcy5hc0lvSGVscGVyKCkuZGVmYXVsdHM7XG4gIH1cblxuICBwdWJsaWMgYXNJb0hlbHBlcigpOiBJb0hlbHBlciB7XG4gICAgcmV0dXJuIGFzSW9IZWxwZXIodGhpcywgdGhpcy5jdXJyZW50QWN0aW9uIGFzIGFueSk7XG4gIH1cblxuICAvKipcbiAgICogRXhlY3V0ZXMgYSBibG9jayBvZiBjb2RlIHdpdGggY29ya2VkIGxvZ2dpbmcuIEFsbCBsb2cgbWVzc2FnZXMgZHVyaW5nIGV4ZWN1dGlvblxuICAgKiBhcmUgYnVmZmVyZWQgYW5kIG9ubHkgd3JpdHRlbiB3aGVuIGFsbCBuZXN0ZWQgY29yayBibG9ja3MgY29tcGxldGUgKHdoZW4gQ09SS19DT1VOVEVSIHJlYWNoZXMgMCkuXG4gICAqIFRoZSBjb3JraW5nIGlzIGJvdW5kIHRvIHRoZSBzcGVjaWZpYyBpbnN0YW5jZSBvZiB0aGUgQ2xpSW9Ib3N0LlxuICAgKlxuICAgKiBAcGFyYW0gYmxvY2sgLSBBc3luYyBmdW5jdGlvbiB0byBleGVjdXRlIHdpdGggY29ya2VkIGxvZ2dpbmdcbiAgICogQHJldHVybnMgUHJvbWlzZSB0aGF0IHJlc29sdmVzIHdpdGggdGhlIGJsb2NrJ3MgcmV0dXJuIHZhbHVlXG4gICAqL1xuICBwdWJsaWMgYXN5bmMgd2l0aENvcmtlZExvZ2dpbmc8VD4oYmxvY2s6ICgpID0+IFByb21pc2U8VD4pOiBQcm9taXNlPFQ+IHtcbiAgICB0aGlzLmNvcmtlZENvdW50ZXIrKztcbiAgICB0cnkge1xuICAgICAgcmV0dXJuIGF3YWl0IGJsb2NrKCk7XG4gICAgfSBmaW5hbGx5IHtcbiAgICAgIHRoaXMuY29ya2VkQ291bnRlci0tO1xuICAgICAgaWYgKHRoaXMuY29ya2VkQ291bnRlciA9PT0gMCkge1xuICAgICAgICAvLyBQcm9jZXNzIGVhY2ggYnVmZmVyZWQgbWVzc2FnZSB0aHJvdWdoIG5vdGlmeVxuICAgICAgICBmb3IgKGNvbnN0IGlvTWVzc2FnZSBvZiB0aGlzLmNvcmtlZExvZ2dpbmdCdWZmZXIpIHtcbiAgICAgICAgICBhd2FpdCB0aGlzLm5vdGlmeShpb01lc3NhZ2UpO1xuICAgICAgICB9XG4gICAgICAgIC8vIHJlbW92ZSBhbGwgYnVmZmVyZWQgbWVzc2FnZXMgaW4tcGxhY2VcbiAgICAgICAgdGhpcy5jb3JrZWRMb2dnaW5nQnVmZmVyLnNwbGljZSgwKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogTm90aWZpZXMgdGhlIGhvc3Qgb2YgYSBtZXNzYWdlLlxuICAgKiBUaGUgY2FsbGVyIHdhaXRzIHVudGlsIHRoZSBub3RpZmljYXRpb24gY29tcGxldGVzLlxuICAgKi9cbiAgcHVibGljIGFzeW5jIG5vdGlmeShtc2c6IElvTWVzc2FnZTx1bmtub3duPik6IFByb21pc2U8dm9pZD4ge1xuICAgIGF3YWl0IHRoaXMubWF5YmVFbWl0VGVsZW1ldHJ5KG1zZyk7XG5cbiAgICBpZiAodGhpcy5pc1N0YWNrQWN0aXZpdHkobXNnKSkge1xuICAgICAgaWYgKCF0aGlzLmFjdGl2aXR5UHJpbnRlcikge1xuICAgICAgICB0aGlzLmFjdGl2aXR5UHJpbnRlciA9IHRoaXMubWFrZUFjdGl2aXR5UHJpbnRlcigpO1xuICAgICAgfVxuICAgICAgdGhpcy5hY3Rpdml0eVByaW50ZXIubm90aWZ5KG1zZyk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKCFpc01lc3NhZ2VSZWxldmFudEZvckxldmVsKG1zZywgdGhpcy5sb2dMZXZlbCkpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBpZiAodGhpcy5jb3JrZWRDb3VudGVyID4gMCkge1xuICAgICAgdGhpcy5jb3JrZWRMb2dnaW5nQnVmZmVyLnB1c2gobXNnKTtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBjb25zdCBvdXRwdXQgPSB0aGlzLmZvcm1hdE1lc3NhZ2UobXNnKTtcbiAgICBjb25zdCBzdHJlYW0gPSB0aGlzLnNlbGVjdFN0cmVhbShtc2cpO1xuICAgIHN0cmVhbT8ud3JpdGUob3V0cHV0KTtcbiAgfVxuXG4gIHByaXZhdGUgYXN5bmMgbWF5YmVFbWl0VGVsZW1ldHJ5KG1zZzogSW9NZXNzYWdlPHVua25vd24+KSB7XG4gICAgdHJ5IHtcbiAgICAgIGlmICh0aGlzLnRlbGVtZXRyeSAmJiBpc1RlbGVtZXRyeU1lc3NhZ2UobXNnKSkge1xuICAgICAgICBhd2FpdCB0aGlzLnRlbGVtZXRyeS5lbWl0KHtcbiAgICAgICAgICBldmVudFR5cGU6IGdldEV2ZW50VHlwZShtc2cpLFxuICAgICAgICAgIGR1cmF0aW9uOiBtc2cuZGF0YS5kdXJhdGlvbixcbiAgICAgICAgICBlcnJvcjogbXNnLmRhdGEuZXJyb3IsXG4gICAgICAgIH0pO1xuICAgICAgfVxuICAgIH0gY2F0Y2ggKGU6IGFueSkge1xuICAgICAgYXdhaXQgdGhpcy5kZWZhdWx0cy50cmFjZShgRW1pdCBUZWxlbWV0cnkgRmFpbGVkICR7ZS5tZXNzYWdlfWApO1xuICAgIH1cbiAgfVxuXG4gIC8qKlxuICAgKiBEZXRlY3Qgc3RhY2sgYWN0aXZpdHkgbWVzc2FnZXMgc28gdGhleSBjYW4gYmUgc2VuZCB0byB0aGUgcHJpbnRlci5cbiAgICovXG4gIHByaXZhdGUgaXNTdGFja0FjdGl2aXR5KG1zZzogSW9NZXNzYWdlPHVua25vd24+KSB7XG4gICAgcmV0dXJuIG1zZy5jb2RlICYmIFtcbiAgICAgICdDREtfVE9PTEtJVF9JNTUwMScsXG4gICAgICAnQ0RLX1RPT0xLSVRfSTU1MDInLFxuICAgICAgJ0NES19UT09MS0lUX0k1NTAzJyxcbiAgICBdLmluY2x1ZGVzKG1zZy5jb2RlKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBEZXRlY3Qgc3BlY2lhbCBtZXNzYWdlcyBlbmNvZGUgaW5mb3JtYXRpb24gYWJvdXQgd2hldGhlciBvciBub3RcbiAgICogdGhleSByZXF1aXJlIGFwcHJvdmFsXG4gICAqL1xuICBwcml2YXRlIHNraXBBcHByb3ZhbFN0ZXAobXNnOiBJb1JlcXVlc3Q8YW55LCBhbnk+KTogYm9vbGVhbiB7XG4gICAgY29uc3QgYXBwcm92YWxUb29sa2l0Q29kZXMgPSBbJ0NES19UT09MS0lUX0k1MDYwJ107XG4gICAgaWYgKCEobXNnLmNvZGUgJiYgYXBwcm92YWxUb29sa2l0Q29kZXMuaW5jbHVkZXMobXNnLmNvZGUpKSkge1xuICAgICAgZmFsc2U7XG4gICAgfVxuXG4gICAgc3dpdGNoICh0aGlzLnJlcXVpcmVEZXBsb3lBcHByb3ZhbCkge1xuICAgICAgLy8gTmV2ZXIgcmVxdWlyZSBhcHByb3ZhbFxuICAgICAgY2FzZSBSZXF1aXJlQXBwcm92YWwuTkVWRVI6XG4gICAgICAgIHJldHVybiB0cnVlO1xuICAgICAgLy8gQWx3YXlzIHJlcXVpcmUgYXBwcm92YWxcbiAgICAgIGNhc2UgUmVxdWlyZUFwcHJvdmFsLkFOWUNIQU5HRTpcbiAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgLy8gUmVxdWlyZSBhcHByb3ZhbCBpZiBjaGFuZ2VzIGluY2x1ZGUgYnJvYWRlbmluZyBwZXJtaXNzaW9uc1xuICAgICAgY2FzZSBSZXF1aXJlQXBwcm92YWwuQlJPQURFTklORzpcbiAgICAgICAgcmV0dXJuIFsnbm9uZScsICdub24tYnJvYWRlbmluZyddLmluY2x1ZGVzKG1zZy5kYXRhPy5wZXJtaXNzaW9uQ2hhbmdlVHlwZSk7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIERldGVybWluZXMgdGhlIG91dHB1dCBzdHJlYW0sIGJhc2VkIG9uIG1lc3NhZ2UgYW5kIGNvbmZpZ3VyYXRpb24uXG4gICAqL1xuICBwcml2YXRlIHNlbGVjdFN0cmVhbShtc2c6IElvTWVzc2FnZTxhbnk+KTogTm9kZUpTLldyaXRlU3RyZWFtIHwgdW5kZWZpbmVkIHtcbiAgICBpZiAoaXNOb3RpY2VzTWVzc2FnZShtc2cpKSB7XG4gICAgICByZXR1cm4gdGFyZ2V0U3RyZWFtT2JqZWN0KHRoaXMubm90aWNlc0Rlc3RpbmF0aW9uKTtcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy5zZWxlY3RTdHJlYW1Gcm9tTGV2ZWwobXNnLmxldmVsKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBEZXRlcm1pbmVzIHRoZSBvdXRwdXQgc3RyZWFtLCBiYXNlZCBvbiBtZXNzYWdlIGxldmVsIGFuZCBjb25maWd1cmF0aW9uLlxuICAgKi9cbiAgcHJpdmF0ZSBzZWxlY3RTdHJlYW1Gcm9tTGV2ZWwobGV2ZWw6IElvTWVzc2FnZUxldmVsKTogTm9kZUpTLldyaXRlU3RyZWFtIHtcbiAgICAvLyBUaGUgc3RyZWFtIHNlbGVjdGlvbiBwb2xpY3kgZm9yIHRoZSBDTEkgaXMgdGhlIGZvbGxvd2luZzpcbiAgICAvL1xuICAgIC8vICAgKDEpIE1lc3NhZ2VzIG9mIGxldmVsIGByZXN1bHRgIGFsd2F5cyBnbyB0byBgc3Rkb3V0YFxuICAgIC8vICAgKDIpIE1lc3NhZ2VzIG9mIGxldmVsIGBlcnJvcmAgYWx3YXlzIGdvIHRvIGBzdGRlcnJgLlxuICAgIC8vICAgKDNhKSBBbGwgcmVtYWluaW5nIG1lc3NhZ2VzIGdvIHRvIGBzdGRlcnJgLlxuICAgIC8vICAgKDNiKSBJZiB3ZSBhcmUgaW4gQ0kgbW9kZSwgYWxsIHJlbWFpbmluZyBtZXNzYWdlcyBnbyB0byBgc3Rkb3V0YC5cbiAgICAvL1xuICAgIHN3aXRjaCAobGV2ZWwpIHtcbiAgICAgIGNhc2UgJ2Vycm9yJzpcbiAgICAgICAgcmV0dXJuIHByb2Nlc3Muc3RkZXJyO1xuICAgICAgY2FzZSAncmVzdWx0JzpcbiAgICAgICAgcmV0dXJuIHByb2Nlc3Muc3Rkb3V0O1xuICAgICAgZGVmYXVsdDpcbiAgICAgICAgcmV0dXJuIHRoaXMuaXNDSSA/IHByb2Nlc3Muc3Rkb3V0IDogcHJvY2Vzcy5zdGRlcnI7XG4gICAgfVxuICB9XG5cbiAgLyoqXG4gICAqIE5vdGlmaWVzIHRoZSBob3N0IG9mIGEgbWVzc2FnZSB0aGF0IHJlcXVpcmVzIGEgcmVzcG9uc2UuXG4gICAqXG4gICAqIElmIHRoZSBob3N0IGRvZXMgbm90IHJldHVybiBhIHJlc3BvbnNlIHRoZSBzdWdnZXN0ZWRcbiAgICogZGVmYXVsdCByZXNwb25zZSBmcm9tIHRoZSBpbnB1dCBtZXNzYWdlIHdpbGwgYmUgdXNlZC5cbiAgICovXG4gIHB1YmxpYyBhc3luYyByZXF1ZXN0UmVzcG9uc2U8RGF0YVR5cGUsIFJlc3BvbnNlVHlwZT4obXNnOiBJb1JlcXVlc3Q8RGF0YVR5cGUsIFJlc3BvbnNlVHlwZT4pOiBQcm9taXNlPFJlc3BvbnNlVHlwZT4ge1xuICAgIC8vIElmIHRoZSByZXF1ZXN0IGNhbm5vdCBiZSBwcm9tcHRlZCBmb3IgYnkgdGhlIENsaUlvSG9zdCwgd2UganVzdCBhY2NlcHQgdGhlIGRlZmF1bHRcbiAgICBpZiAoIWlzUHJvbXB0YWJsZVJlcXVlc3QobXNnKSkge1xuICAgICAgYXdhaXQgdGhpcy5ub3RpZnkobXNnKTtcbiAgICAgIHJldHVybiBtc2cuZGVmYXVsdFJlc3BvbnNlO1xuICAgIH1cblxuICAgIGNvbnN0IHJlc3BvbnNlID0gYXdhaXQgdGhpcy53aXRoQ29ya2VkTG9nZ2luZyhhc3luYyAoKTogUHJvbWlzZTxzdHJpbmcgfCBudW1iZXIgfCB0cnVlPiA9PiB7XG4gICAgICAvLyBwcmVwYXJlIHByb21wdCBkYXRhXG4gICAgICAvLyBAdG9kbyB0aGlzIGZvcm1hdCBpcyBub3QgZGVmaW5lZCBhbnl3aGVyZSwgcHJvYmFibHkgc2hvdWxkIGJlXG4gICAgICBjb25zdCBkYXRhOiB7XG4gICAgICAgIG1vdGl2YXRpb24/OiBzdHJpbmc7XG4gICAgICAgIGNvbmN1cnJlbmN5PzogbnVtYmVyO1xuICAgICAgICByZXNwb25zZURlc2NyaXB0aW9uPzogc3RyaW5nO1xuICAgICAgfSA9IG1zZy5kYXRhID8/IHt9O1xuXG4gICAgICBjb25zdCBtb3RpdmF0aW9uID0gZGF0YS5tb3RpdmF0aW9uID8/ICdVc2VyIGlucHV0IGlzIG5lZWRlZCc7XG4gICAgICBjb25zdCBjb25jdXJyZW5jeSA9IGRhdGEuY29uY3VycmVuY3kgPz8gMDtcbiAgICAgIGNvbnN0IHJlc3BvbnNlRGVzY3JpcHRpb24gPSBkYXRhLnJlc3BvbnNlRGVzY3JpcHRpb247XG5cbiAgICAgIC8vIG9ubHkgdGFsayB0byB1c2VyIGlmIFNURElOIGlzIGEgdGVybWluYWwgKG90aGVyd2lzZSwgZmFpbClcbiAgICAgIGlmICghdGhpcy5pc1RUWSkge1xuICAgICAgICB0aHJvdyBuZXcgVG9vbGtpdEVycm9yKGAke21vdGl2YXRpb259LCBidXQgdGVybWluYWwgKFRUWSkgaXMgbm90IGF0dGFjaGVkIHNvIHdlIGFyZSB1bmFibGUgdG8gZ2V0IGEgY29uZmlybWF0aW9uIGZyb20gdGhlIHVzZXJgKTtcbiAgICAgIH1cblxuICAgICAgLy8gb25seSB0YWxrIHRvIHVzZXIgaWYgY29uY3VycmVuY3kgaXMgMSAob3RoZXJ3aXNlLCBmYWlsKVxuICAgICAgaWYgKGNvbmN1cnJlbmN5ID4gMSkge1xuICAgICAgICB0aHJvdyBuZXcgVG9vbGtpdEVycm9yKGAke21vdGl2YXRpb259LCBidXQgY29uY3VycmVuY3kgaXMgZ3JlYXRlciB0aGFuIDEgc28gd2UgYXJlIHVuYWJsZSB0byBnZXQgYSBjb25maXJtYXRpb24gZnJvbSB0aGUgdXNlcmApO1xuICAgICAgfVxuXG4gICAgICAvLyBTcGVjaWFsIGFwcHJvdmFsIHByb21wdFxuICAgICAgLy8gRGV0ZXJtaW5lIGlmIHRoZSBtZXNzYWdlIG5lZWRzIGFwcHJvdmFsLiBJZiBpdCBkb2VzLCBjb250aW51ZSAoaXQgaXMgYSBiYXNpYyBjb25maXJtYXRpb24gcHJvbXB0KVxuICAgICAgLy8gSWYgaXQgZG9lcyBub3QsIHJldHVybiBzdWNjZXNzICh0cnVlKS4gV2Ugb25seSBjaGVjayBtZXNzYWdlcyB3aXRoIGNvZGVzIHRoYXQgd2UgYXJlIGF3YXJlXG4gICAgICAvLyBhcmUgcmVxdWlyZXMgYXBwcm92YWwgY29kZXMuXG4gICAgICBpZiAodGhpcy5za2lwQXBwcm92YWxTdGVwKG1zZykpIHtcbiAgICAgICAgcmV0dXJuIHRydWU7XG4gICAgICB9XG5cbiAgICAgIC8vIEJhc2ljIGNvbmZpcm1hdGlvbiBwcm9tcHRcbiAgICAgIC8vIFdlIHRyZWF0IGFsbCByZXF1ZXN0cyB3aXRoIGEgYm9vbGVhbiByZXNwb25zZSBhcyBjb25maXJtYXRpb24gcHJvbXB0c1xuICAgICAgaWYgKGlzQ29uZmlybWF0aW9uUHJvbXB0KG1zZykpIHtcbiAgICAgICAgY29uc3QgY29uZmlybWVkID0gYXdhaXQgcHJvbXB0bHkuY29uZmlybShgJHtjaGFsay5jeWFuKG1zZy5tZXNzYWdlKX0gKHkvbilgKTtcbiAgICAgICAgaWYgKCFjb25maXJtZWQpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgVG9vbGtpdEVycm9yKCdBYm9ydGVkIGJ5IHVzZXInKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gY29uZmlybWVkO1xuICAgICAgfVxuXG4gICAgICAvLyBBc2tpbmcgZm9yIGEgc3BlY2lmaWMgdmFsdWVcbiAgICAgIGNvbnN0IHByb21wdCA9IGV4dHJhY3RQcm9tcHRJbmZvKG1zZyk7XG4gICAgICBjb25zdCBkZXNjID0gcmVzcG9uc2VEZXNjcmlwdGlvbiA/PyBwcm9tcHQuZGVmYXVsdDtcbiAgICAgIGNvbnN0IGFuc3dlciA9IGF3YWl0IHByb21wdGx5LnByb21wdChgJHtjaGFsay5jeWFuKG1zZy5tZXNzYWdlKX0ke2Rlc2MgPyBgICgke2Rlc2N9KWAgOiAnJ31gLCB7XG4gICAgICAgIGRlZmF1bHQ6IHByb21wdC5kZWZhdWx0LFxuICAgICAgICB0cmltOiB0cnVlLFxuICAgICAgfSk7XG4gICAgICByZXR1cm4gcHJvbXB0LmNvbnZlcnRBbnN3ZXIoYW5zd2VyKTtcbiAgICB9KTtcblxuICAgIC8vIFdlIG5lZWQgdG8gY2FzdCB0aGlzIGJlY2F1c2UgaXQgaXMgaW1wb3NzaWJsZSB0byBuYXJyb3cgdGhlIGdlbmVyaWMgdHlwZVxuICAgIC8vIGlzUHJvbXB0YWJsZVJlcXVlc3QgZW5zdXJlcyB0aGF0IHRoZSByZXNwb25zZSB0eXBlIGlzIG9uZSB3ZSBjYW4gcHJvbXB0IGZvclxuICAgIC8vIHRoZSByZW1haW5pbmcgY29kZSBlbnN1cmUgd2UgYXJlIGluZGVlZCByZXR1cm5pbmcgdGhlIGNvcnJlY3QgdHlwZVxuICAgIHJldHVybiByZXNwb25zZSBhcyBSZXNwb25zZVR5cGU7XG4gIH1cblxuICAvKipcbiAgICogRm9ybWF0cyBhIG1lc3NhZ2UgZm9yIGNvbnNvbGUgb3V0cHV0IHdpdGggb3B0aW9uYWwgY29sb3Igc3VwcG9ydFxuICAgKi9cbiAgcHJpdmF0ZSBmb3JtYXRNZXNzYWdlKG1zZzogSW9NZXNzYWdlPHVua25vd24+KTogc3RyaW5nIHtcbiAgICAvLyBhcHBseSBwcm92aWRlZCBzdHlsZSBvciBhIGRlZmF1bHQgc3R5bGUgaWYgd2UncmUgaW4gVFRZIG1vZGVcbiAgICBsZXQgbWVzc2FnZV90ZXh0ID0gdGhpcy5pc1RUWVxuICAgICAgPyBzdHlsZU1hcFttc2cubGV2ZWxdKG1zZy5tZXNzYWdlKVxuICAgICAgOiBtc2cubWVzc2FnZTtcblxuICAgIC8vIHByZXBlbmQgdGltZXN0YW1wIGlmIElvTWVzc2FnZUxldmVsIGlzIERFQlVHIG9yIFRSQUNFLiBQb3N0cGVuZCBhIG5ld2xpbmUuXG4gICAgcmV0dXJuICgobXNnLmxldmVsID09PSAnZGVidWcnIHx8IG1zZy5sZXZlbCA9PT0gJ3RyYWNlJylcbiAgICAgID8gYFske3RoaXMuZm9ybWF0VGltZShtc2cudGltZSl9XSAke21lc3NhZ2VfdGV4dH1gXG4gICAgICA6IG1lc3NhZ2VfdGV4dCkgKyAnXFxuJztcbiAgfVxuXG4gIC8qKlxuICAgKiBGb3JtYXRzIGRhdGUgdG8gSEg6TU06U1NcbiAgICovXG4gIHByaXZhdGUgZm9ybWF0VGltZShkOiBEYXRlKTogc3RyaW5nIHtcbiAgICBjb25zdCBwYWQgPSAobjogbnVtYmVyKTogc3RyaW5nID0+IG4udG9TdHJpbmcoKS5wYWRTdGFydCgyLCAnMCcpO1xuICAgIHJldHVybiBgJHtwYWQoZC5nZXRIb3VycygpKX06JHtwYWQoZC5nZXRNaW51dGVzKCkpfToke3BhZChkLmdldFNlY29uZHMoKSl9YDtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXQgYW4gaW5zdGFuY2Ugb2YgdGhlIEFjdGl2aXR5UHJpbnRlclxuICAgKi9cbiAgcHJpdmF0ZSBtYWtlQWN0aXZpdHlQcmludGVyKCkge1xuICAgIGNvbnN0IHByb3BzOiBBY3Rpdml0eVByaW50ZXJQcm9wcyA9IHtcbiAgICAgIHN0cmVhbTogdGhpcy5zZWxlY3RTdHJlYW1Gcm9tTGV2ZWwoJ2luZm8nKSxcbiAgICB9O1xuXG4gICAgc3dpdGNoICh0aGlzLnN0YWNrUHJvZ3Jlc3MpIHtcbiAgICAgIGNhc2UgU3RhY2tBY3Rpdml0eVByb2dyZXNzLkVWRU5UUzpcbiAgICAgICAgcmV0dXJuIG5ldyBIaXN0b3J5QWN0aXZpdHlQcmludGVyKHByb3BzKTtcbiAgICAgIGNhc2UgU3RhY2tBY3Rpdml0eVByb2dyZXNzLkJBUjpcbiAgICAgICAgcmV0dXJuIG5ldyBDdXJyZW50QWN0aXZpdHlQcmludGVyKHByb3BzKTtcbiAgICB9XG4gIH1cbn1cblxuLyoqXG4gKiBUaGlzIElvSG9zdCBpbXBsZW1lbnRhdGlvbiBjb25zaWRlcnMgYSByZXF1ZXN0IHByb21wdGFibGUsIGlmOlxuICogLSBpdCdzIGEgeWVzL25vIGNvbmZpcm1hdGlvblxuICogLSBhc2tpbmcgZm9yIGEgc3RyaW5nIG9yIG51bWJlciB2YWx1ZVxuICovXG5mdW5jdGlvbiBpc1Byb21wdGFibGVSZXF1ZXN0KG1zZzogSW9SZXF1ZXN0PGFueSwgYW55Pik6IG1zZyBpcyBJb1JlcXVlc3Q8YW55LCBzdHJpbmcgfCBudW1iZXIgfCBib29sZWFuPiB7XG4gIHJldHVybiBpc0NvbmZpcm1hdGlvblByb21wdChtc2cpXG4gICAgfHwgdHlwZW9mIG1zZy5kZWZhdWx0UmVzcG9uc2UgPT09ICdzdHJpbmcnXG4gICAgfHwgdHlwZW9mIG1zZy5kZWZhdWx0UmVzcG9uc2UgPT09ICdudW1iZXInO1xufVxuXG4vKipcbiAqIENoZWNrIGlmIHRoZSByZXF1ZXN0IGlzIGEgY29uZmlybWF0aW9uIHByb21wdFxuICogV2UgdHJlYXQgYWxsIHJlcXVlc3RzIHdpdGggYSBib29sZWFuIHJlc3BvbnNlIGFzIGNvbmZpcm1hdGlvbiBwcm9tcHRzXG4gKi9cbmZ1bmN0aW9uIGlzQ29uZmlybWF0aW9uUHJvbXB0KG1zZzogSW9SZXF1ZXN0PGFueSwgYW55Pik6IG1zZyBpcyBJb1JlcXVlc3Q8YW55LCBib29sZWFuPiB7XG4gIHJldHVybiB0eXBlb2YgbXNnLmRlZmF1bHRSZXNwb25zZSA9PT0gJ2Jvb2xlYW4nO1xufVxuXG4vKipcbiAqIEhlbHBlciB0byBleHRyYWN0IGluZm9ybWF0aW9uIGZvciBwcm9tcHRseSBmcm9tIHRoZSByZXF1ZXN0XG4gKi9cbmZ1bmN0aW9uIGV4dHJhY3RQcm9tcHRJbmZvKG1zZzogSW9SZXF1ZXN0PGFueSwgYW55Pik6IHtcbiAgZGVmYXVsdDogc3RyaW5nO1xuICBkZWZhdWx0RGVzYzogc3RyaW5nO1xuICBjb252ZXJ0QW5zd2VyOiAoaW5wdXQ6IHN0cmluZykgPT4gc3RyaW5nIHwgbnVtYmVyO1xufSB7XG4gIGNvbnN0IGlzTnVtYmVyID0gKHR5cGVvZiBtc2cuZGVmYXVsdFJlc3BvbnNlID09PSAnbnVtYmVyJyk7XG4gIGNvbnN0IGRlZmF1bHRSZXNwb25zZSA9IHV0aWwuZm9ybWF0KG1zZy5kZWZhdWx0UmVzcG9uc2UpO1xuICByZXR1cm4ge1xuICAgIGRlZmF1bHQ6IGRlZmF1bHRSZXNwb25zZSxcbiAgICBkZWZhdWx0RGVzYzogJ2RlZmF1bHREZXNjcmlwdGlvbicgaW4gbXNnICYmIG1zZy5kZWZhdWx0RGVzY3JpcHRpb24gPyB1dGlsLmZvcm1hdChtc2cuZGVmYXVsdERlc2NyaXB0aW9uKSA6IGRlZmF1bHRSZXNwb25zZSxcbiAgICBjb252ZXJ0QW5zd2VyOiBpc051bWJlciA/ICh2KSA9PiBOdW1iZXIodikgOiAodikgPT4gU3RyaW5nKHYpLFxuICB9O1xufVxuXG5jb25zdCBzdHlsZU1hcDogUmVjb3JkPElvTWVzc2FnZUxldmVsLCAoc3RyOiBzdHJpbmcpID0+IHN0cmluZz4gPSB7XG4gIGVycm9yOiBjaGFsay5yZWQsXG4gIHdhcm46IGNoYWxrLnllbGxvdyxcbiAgcmVzdWx0OiBjaGFsay5yZXNldCxcbiAgaW5mbzogY2hhbGsucmVzZXQsXG4gIGRlYnVnOiBjaGFsay5ncmF5LFxuICB0cmFjZTogY2hhbGsuZ3JheSxcbn07XG5cbmZ1bmN0aW9uIHRhcmdldFN0cmVhbU9iamVjdCh4OiBUYXJnZXRTdHJlYW0pOiBOb2RlSlMuV3JpdGVTdHJlYW0gfCB1bmRlZmluZWQge1xuICBzd2l0Y2ggKHgpIHtcbiAgICBjYXNlICdzdGRlcnInOlxuICAgICAgcmV0dXJuIHByb2Nlc3Muc3RkZXJyO1xuICAgIGNhc2UgJ3N0ZG91dCc6XG4gICAgICByZXR1cm4gcHJvY2Vzcy5zdGRvdXQ7XG4gICAgY2FzZSAnZHJvcCc6XG4gICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICB9XG59XG5cbmZ1bmN0aW9uIGlzTm90aWNlc01lc3NhZ2UobXNnOiBJb01lc3NhZ2U8dW5rbm93bj4pOiBtc2cgaXMgSW9NZXNzYWdlPHZvaWQ+IHtcbiAgcmV0dXJuIElPLkNES19UT09MS0lUX0kwMTAwLmlzKG1zZykgfHwgSU8uQ0RLX1RPT0xLSVRfVzAxMDEuaXMobXNnKSB8fCBJTy5DREtfVE9PTEtJVF9FMDEwMS5pcyhtc2cpIHx8IElPLkNES19UT09MS0lUX0kwMTAxLmlzKG1zZyk7XG59XG5cbmZ1bmN0aW9uIGlzVGVsZW1ldHJ5TWVzc2FnZShtc2c6IElvTWVzc2FnZTx1bmtub3duPik6IG1zZyBpcyBJb01lc3NhZ2U8RXZlbnRSZXN1bHQ+IHtcbiAgcmV0dXJuIENMSV9URUxFTUVUUllfQ09ERVMuc29tZSgoYykgPT4gYy5pcyhtc2cpKTtcbn1cblxuZnVuY3Rpb24gZ2V0RXZlbnRUeXBlKG1zZzogSW9NZXNzYWdlPHVua25vd24+KTogRXZlbnRUeXBlIHtcbiAgc3dpdGNoIChtc2cuY29kZSkge1xuICAgIGNhc2UgQ0xJX1BSSVZBVEVfSU8uQ0RLX0NMSV9JMTAwMS5jb2RlOlxuICAgICAgcmV0dXJuICdTWU5USCc7XG4gICAgY2FzZSBDTElfUFJJVkFURV9JTy5DREtfQ0xJX0kyMDAxLmNvZGU6XG4gICAgICByZXR1cm4gJ0lOVk9LRSc7XG4gICAgY2FzZSBDTElfUFJJVkFURV9JTy5DREtfQ0xJX0kzMDAxLmNvZGU6XG4gICAgICByZXR1cm4gJ0RFUExPWSc7XG4gICAgZGVmYXVsdDpcbiAgICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoYFVucmVjb2duaXplZCBUZWxlbWV0cnkgTWVzc2FnZSBDb2RlOiAke21zZy5jb2RlfWApO1xuICB9XG59XG4iXX0=