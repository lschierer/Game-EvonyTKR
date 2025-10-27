"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.execProgram = execProgram;
exports.createAssembly = createAssembly;
const childProcess = require("child_process");
const util_1 = require("util");
const cxschema = require("@aws-cdk/cloud-assembly-schema");
const cxapi = require("@aws-cdk/cx-api");
const toolkit_lib_1 = require("@aws-cdk/toolkit-lib");
const fs = require("fs-extra");
const api_1 = require("../api");
const user_configuration_1 = require("../cli/user-configuration");
const version_1 = require("../cli/version");
/** Invokes the cloud executable and returns JSON output */
async function execProgram(aws, ioHelper, config) {
    const debugFn = (msg) => ioHelper.defaults.debug(msg);
    const params = (0, api_1.synthParametersFromSettings)(config.settings);
    const context = {
        ...config.context.all,
        ...params.context,
    };
    await debugFn((0, util_1.format)('context:', context));
    const env = noUndefined({
        // Versioning, outdir, default account and region
        ...await (0, api_1.prepareDefaultEnvironment)(aws, debugFn),
        // Environment variables derived from settings
        ...params.env,
    });
    const build = config.settings.get(['build']);
    if (build) {
        await exec(build);
    }
    const app = config.settings.get(['app']);
    if (!app) {
        throw new toolkit_lib_1.ToolkitError(`--app is required either in command-line, in ${user_configuration_1.PROJECT_CONFIG} or in ${user_configuration_1.USER_DEFAULTS}`);
    }
    // bypass "synth" if app points to a cloud assembly
    if (await fs.pathExists(app) && (await fs.stat(app)).isDirectory()) {
        await debugFn('--app points to a cloud assembly, so we bypass synth');
        // Acquire a read lock on this directory
        const lock = await new api_1.RWLock(app).acquireRead();
        return { assembly: createAssembly(app), lock };
    }
    const commandLine = await (0, api_1.guessExecutable)(app, debugFn);
    const outdir = config.settings.get(['output']);
    if (!outdir) {
        throw new toolkit_lib_1.ToolkitError('unexpected: --output is required');
    }
    if (typeof outdir !== 'string') {
        throw new toolkit_lib_1.ToolkitError(`--output takes a string, got ${JSON.stringify(outdir)}`);
    }
    try {
        await fs.mkdirp(outdir);
    }
    catch (error) {
        throw new toolkit_lib_1.ToolkitError(`Could not create output directory ${outdir} (${error.message})`);
    }
    await debugFn(`outdir: ${outdir}`);
    env[cxapi.OUTDIR_ENV] = outdir;
    // Acquire a lock on the output directory
    const writerLock = await new api_1.RWLock(outdir).acquireWrite();
    // Send version information
    env[cxapi.CLI_ASM_VERSION_ENV] = cxschema.Manifest.version();
    env[cxapi.CLI_VERSION_ENV] = (0, version_1.versionNumber)();
    await debugFn((0, util_1.format)('env:', env));
    const cleanupTemp = (0, api_1.writeContextToEnv)(env, context, 'add-process-env-later');
    try {
        await exec(commandLine.join(' '));
        const assembly = createAssembly(outdir);
        return { assembly, lock: await writerLock.convertToReaderLock() };
    }
    catch (e) {
        await writerLock.release();
        throw e;
    }
    finally {
        await cleanupTemp();
    }
    async function exec(commandAndArgs) {
        try {
            await new Promise((ok, fail) => {
                // We use a slightly lower-level interface to:
                //
                // - Pass arguments in an array instead of a string, to get around a
                //   number of quoting issues introduced by the intermediate shell layer
                //   (which would be different between Linux and Windows).
                //
                // - Inherit stderr from controlling terminal. We don't use the captured value
                //   anyway, and if the subprocess is printing to it for debugging purposes the
                //   user gets to see it sooner. Plus, capturing doesn't interact nicely with some
                //   processes like Maven.
                const proc = childProcess.spawn(commandAndArgs, {
                    stdio: ['ignore', 'inherit', 'inherit'],
                    detached: false,
                    shell: true,
                    env: {
                        ...process.env,
                        ...env,
                    },
                });
                proc.on('error', fail);
                proc.on('exit', code => {
                    if (code === 0) {
                        return ok();
                    }
                    else {
                        return fail(new toolkit_lib_1.ToolkitError(`${commandAndArgs}: Subprocess exited with error ${code}`));
                    }
                });
            });
        }
        catch (e) {
            await debugFn(`failed command: ${commandAndArgs}`);
            throw e;
        }
    }
}
/**
 * Creates an assembly with error handling
 */
function createAssembly(appDir) {
    try {
        return new cxapi.CloudAssembly(appDir, {
            // We sort as we deploy
            topoSort: false,
        });
    }
    catch (error) {
        if (error.message.includes(cxschema.VERSION_MISMATCH)) {
            // this means the CLI version is too old.
            // we instruct the user to upgrade.
            throw new toolkit_lib_1.ToolkitError(`This CDK CLI is not compatible with the CDK library used by your application. Please upgrade the CLI to the latest version.\n(${error.message})`);
        }
        throw error;
    }
}
function noUndefined(xs) {
    return Object.fromEntries(Object.entries(xs).filter(([_, v]) => v !== undefined));
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXhlYy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbImV4ZWMudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7QUFtQkEsa0NBc0hDO0FBS0Qsd0NBY0M7QUE1SkQsOENBQThDO0FBQzlDLCtCQUE4QjtBQUM5QiwyREFBMkQ7QUFDM0QseUNBQXlDO0FBQ3pDLHNEQUFvRDtBQUNwRCwrQkFBK0I7QUFHL0IsZ0NBQTRIO0FBRTVILGtFQUEwRTtBQUMxRSw0Q0FBK0M7QUFPL0MsMkRBQTJEO0FBQ3BELEtBQUssVUFBVSxXQUFXLENBQUMsR0FBZ0IsRUFBRSxRQUFrQixFQUFFLE1BQXFCO0lBQzNGLE1BQU0sT0FBTyxHQUFHLENBQUMsR0FBVyxFQUFFLEVBQUUsQ0FBQyxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsQ0FBQztJQUU5RCxNQUFNLE1BQU0sR0FBRyxJQUFBLGlDQUEyQixFQUFDLE1BQU0sQ0FBQyxRQUFRLENBQUMsQ0FBQztJQUU1RCxNQUFNLE9BQU8sR0FBRztRQUNkLEdBQUcsTUFBTSxDQUFDLE9BQU8sQ0FBQyxHQUFHO1FBQ3JCLEdBQUcsTUFBTSxDQUFDLE9BQU87S0FDbEIsQ0FBQztJQUNGLE1BQU0sT0FBTyxDQUFDLElBQUEsYUFBTSxFQUFDLFVBQVUsRUFBRSxPQUFPLENBQUMsQ0FBQyxDQUFDO0lBRTNDLE1BQU0sR0FBRyxHQUEyQixXQUFXLENBQUM7UUFDOUMsaURBQWlEO1FBQ2pELEdBQUcsTUFBTSxJQUFBLCtCQUF5QixFQUFDLEdBQUcsRUFBRSxPQUFPLENBQUM7UUFDaEQsOENBQThDO1FBQzlDLEdBQUcsTUFBTSxDQUFDLEdBQUc7S0FDZCxDQUFDLENBQUM7SUFFSCxNQUFNLEtBQUssR0FBRyxNQUFNLENBQUMsUUFBUSxDQUFDLEdBQUcsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUM7SUFDN0MsSUFBSSxLQUFLLEVBQUUsQ0FBQztRQUNWLE1BQU0sSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFD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QUNoQixNQUFNLE9BQU8sQ0FBQyxtQkFBbUIsY0FBYyxFQUFFLENBQUMsQ0FBQztZQUNuRCxNQUFNLENBQUMsQ0FBQztRQUNWLENBQUM7SUFDSCxDQUFDO0FBQ0gsQ0FBQztBQUVEOztHQUVHO0FBQ0gsU0FBZ0IsY0FBYyxDQUFDLE1BQWM7SUFDM0MsSUFBSSxDQUFDO1FBQ0gsT0FBTyxJQUFJLEtBQUssQ0FBQyxhQUFhLENBQUMsTUFBTSxFQUFFO1lBQ3JDLHVCQUF1QjtZQUN2QixRQUFRLEVBQUUsS0FBSztTQUNoQixDQUFDLENBQUM7SUFDTCxDQUFDO0lBQUMsT0FBTyxLQUFVLEVBQUUsQ0FBQztRQUNwQixJQUFJLEtBQUssQ0FBQyxPQUFPLENBQUMsUUFBUSxDQUFDLFFBQVEsQ0FBQyxnQkFBZ0IsQ0FBQyxFQUFFLENBQUM7WUFDdEQseUNBQXlDO1lBQ3pDLG1DQUFtQztZQUNuQyxNQUFNLElBQUksMEJBQVksQ0FBQyxpSUFBaUksS0FBSyxDQUFDLE9BQU8sR0FBRyxDQUFDLENBQUM7UUFDNUssQ0FBQztRQUNELE1BQU0sS0FBSyxDQUFDO0lBQ2QsQ0FBQztBQUNILENBQUM7QUFFRCxTQUFTLFdBQVcsQ0FBSSxFQUFxQjtJQUMzQyxPQUFPLE1BQU0sQ0FBQyxXQUFXLENBQUMsTUFBTSxDQUFDLE9BQU8sQ0FBQyxFQUFFLENBQUMsQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsRUFBRSxFQUFFLENBQUMsQ0FBQyxLQUFLLFNBQVMsQ0FBQyxDQUFRLENBQUM7QUFDM0YsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCAqIGFzIGNoaWxkUHJvY2VzcyBmcm9tICdjaGlsZF9wcm9jZXNzJztcbmltcG9ydCB7IGZvcm1hdCB9IGZyb20gJ3V0aWwnO1xuaW1wb3J0ICogYXMgY3hzY2hlbWEgZnJvbSAnQGF3cy1jZGsvY2xvdWQtYXNzZW1ibHktc2NoZW1hJztcbmltcG9ydCAqIGFzIGN4YXBpIGZyb20gJ0Bhd3MtY2RrL2N4LWFwaSc7XG5pbXBvcnQgeyBUb29sa2l0RXJyb3IgfSBmcm9tICdAYXdzLWNkay90b29sa2l0LWxpYic7XG5pbXBvcnQgKiBhcyBmcyBmcm9tICdmcy1leHRyYSc7XG5pbXBvcnQgdHlwZSB7IElvSGVscGVyIH0gZnJvbSAnLi4vLi4vbGliL2FwaS1wcml2YXRlJztcbmltcG9ydCB0eXBlIHsgU2RrUHJvdmlkZXIsIElSZWFkTG9jayB9IGZyb20gJy4uL2FwaSc7XG5pbXBvcnQgeyBSV0xvY2ssIGd1ZXNzRXhlY3V0YWJsZSwgcHJlcGFyZURlZmF1bHRFbnZpcm9ubWVudCwgd3JpdGVDb250ZXh0VG9FbnYsIHN5bnRoUGFyYW1ldGVyc0Zyb21TZXR0aW5ncyB9IGZyb20gJy4uL2FwaSc7XG5pbXBvcnQgdHlwZSB7IENvbmZpZ3VyYXRpb24gfSBmcm9tICcuLi9jbGkvdXNlci1jb25maWd1cmF0aW9uJztcbmltcG9ydCB7IFBST0pFQ1RfQ09ORklHLCBVU0VSX0RFRkFVTFRTIH0gZnJvbSAnLi4vY2xpL3VzZXItY29uZmlndXJhdGlvbic7XG5pbXBvcnQgeyB2ZXJzaW9uTnVtYmVyIH0gZnJvbSAnLi4vY2xpL3ZlcnNpb24nO1xuXG5leHBvcnQgaW50ZXJmYWNlIEV4ZWNQcm9ncmFtUmVzdWx0IHtcbiAgcmVhZG9ubHkgYXNzZW1ibHk6IGN4YXBpLkNsb3VkQXNzZW1ibHk7XG4gIHJlYWRvbmx5IGxvY2s6IElSZWFkTG9jaztcbn1cblxuLyoqIEludm9rZXMgdGhlIGNsb3VkIGV4ZWN1dGFibGUgYW5kIHJldHVybnMgSlNPTiBvdXRwdXQgKi9cbmV4cG9ydCBhc3luYyBmdW5jdGlvbiBleGVjUHJvZ3JhbShhd3M6IFNka1Byb3ZpZGVyLCBpb0hlbHBlcjogSW9IZWxwZXIsIGNvbmZpZzogQ29uZmlndXJhdGlvbik6IFByb21pc2U8RXhlY1Byb2dyYW1SZXN1bHQ+IHtcbiAgY29uc3QgZGVidWdGbiA9IChtc2c6IHN0cmluZykgPT4gaW9IZWxwZXIuZGVmYXVsdHMuZGVidWcobXNnKTtcblxuICBjb25zdCBwYXJhbXMgPSBzeW50aFBhcmFtZXRlcnNGcm9tU2V0dGluZ3MoY29uZmlnLnNldHRpbmdzKTtcblxuICBjb25zdCBjb250ZXh0ID0ge1xuICAgIC4uLmNvbmZpZy5jb250ZXh0LmFsbCxcbiAgICAuLi5wYXJhbXMuY29udGV4dCxcbiAgfTtcbiAgYXdhaXQgZGVidWdGbihmb3JtYXQoJ2NvbnRleHQ6JywgY29udGV4dCkpO1xuXG4gIGNvbnN0IGVudjogUmVjb3JkPHN0cmluZywgc3RyaW5nPiA9IG5vVW5kZWZpbmVkKHtcbiAgICAvLyBWZXJzaW9uaW5nLCBvdXRkaXIsIGRlZmF1bHQgYWNjb3VudCBhbmQgcmVnaW9uXG4gICAgLi4uYXdhaXQgcHJlcGFyZURlZmF1bHRFbnZpcm9ubWVudChhd3MsIGRlYnVnRm4pLFxuICAgIC8vIEVudmlyb25tZW50IHZhcmlhYmxlcyBkZXJpdmVkIGZyb20gc2V0dGluZ3NcbiAgICAuLi5wYXJhbXMuZW52LFxuICB9KTtcblxuICBjb25zdCBidWlsZCA9IGNvbmZpZy5zZXR0aW5ncy5nZXQoWydidWlsZCddKTtcbiAgaWYgKGJ1aWxkKSB7XG4gICAgYXdhaXQgZXhlYyhidWlsZCk7XG4gIH1cblxuICBjb25zdCBhcHAgPSBjb25maWcuc2V0dGluZ3MuZ2V0KFsnYXBwJ10pO1xuICBpZiAoIWFwcCkge1xuICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoYC0tYXBwIGlzIHJlcXVpcmVkIGVpdGhlciBpbiBjb21tYW5kLWxpbmUsIGluICR7UFJPSkVDVF9DT05GSUd9IG9yIGluICR7VVNFUl9ERUZBVUxUU31gKTtcbiAgfVxuXG4gIC8vIGJ5cGFzcyBcInN5bnRoXCIgaWYgYXBwIHBvaW50cyB0byBhIGNsb3VkIGFzc2VtYmx5XG4gIGlmIChhd2FpdCBmcy5wYXRoRXhpc3RzKGFwcCkgJiYgKGF3YWl0IGZzLnN0YXQoYXBwKSkuaXNEaXJlY3RvcnkoKSkge1xuICAgIGF3YWl0IGRlYnVnRm4oJy0tYXBwIHBvaW50cyB0byBhIGNsb3VkIGFzc2VtYmx5LCBzbyB3ZSBieXBhc3Mgc3ludGgnKTtcblxuICAgIC8vIEFjcXVpcmUgYSByZWFkIGxvY2sgb24gdGhpcyBkaXJlY3RvcnlcbiAgICBjb25zdCBsb2NrID0gYXdhaXQgbmV3IFJXTG9jayhhcHApLmFjcXVpcmVSZWFkKCk7XG5cbiAgICByZXR1cm4geyBhc3NlbWJseTogY3JlYXRlQXNzZW1ibHkoYXBwKSwgbG9jayB9O1xuICB9XG5cbiAgY29uc3QgY29tbWFuZExpbmUgPSBhd2FpdCBndWVzc0V4ZWN1dGFibGUoYXBwLCBkZWJ1Z0ZuKTtcblxuICBjb25zdCBvdXRkaXIgPSBjb25maWcuc2V0dGluZ3MuZ2V0KFsnb3V0cHV0J10pO1xuICBpZiAoIW91dGRpcikge1xuICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoJ3VuZXhwZWN0ZWQ6IC0tb3V0cHV0IGlzIHJlcXVpcmVkJyk7XG4gIH1cbiAgaWYgKHR5cGVvZiBvdXRkaXIgIT09ICdzdHJpbmcnKSB7XG4gICAgdGhyb3cgbmV3IFRvb2xraXRFcnJvcihgLS1vdXRwdXQgdGFrZXMgYSBzdHJpbmcsIGdvdCAke0pTT04uc3RyaW5naWZ5KG91dGRpcil9YCk7XG4gIH1cbiAgdHJ5IHtcbiAgICBhd2FpdCBmcy5ta2RpcnAob3V0ZGlyKTtcbiAgfSBjYXRjaCAoZXJyb3I6IGFueSkge1xuICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoYENvdWxkIG5vdCBjcmVhdGUgb3V0cHV0IGRpcmVjdG9yeSAke291dGRpcn0gKCR7ZXJyb3IubWVzc2FnZX0pYCk7XG4gIH1cblxuICBhd2FpdCBkZWJ1Z0ZuKGBvdXRkaXI6ICR7b3V0ZGlyfWApO1xuXG4gIGVudltjeGFwaS5PVVRESVJfRU5WXSA9IG91dGRpcjtcblxuICAvLyBBY3F1aXJlIGEgbG9jayBvbiB0aGUgb3V0cHV0IGRpcmVjdG9yeVxuICBjb25zdCB3cml0ZXJMb2NrID0gYXdhaXQgbmV3IFJXTG9jayhvdXRkaXIpLmFjcXVpcmVXcml0ZSgpO1xuXG4gIC8vIFNlbmQgdmVyc2lvbiBpbmZvcm1hdGlvblxuICBlbnZbY3hhcGkuQ0xJX0FTTV9WRVJTSU9OX0VOVl0gPSBjeHNjaGVtYS5NYW5pZmVzdC52ZXJzaW9uKCk7XG4gIGVudltjeGFwaS5DTElfVkVSU0lPTl9FTlZdID0gdmVyc2lvbk51bWJlcigpO1xuXG4gIGF3YWl0IGRlYnVnRm4oZm9ybWF0KCdlbnY6JywgZW52KSk7XG5cbiAgY29uc3QgY2xlYW51cFRlbXAgPSB3cml0ZUNvbnRleHRUb0VudihlbnYsIGNvbnRleHQsICdhZGQtcHJvY2Vzcy1lbnYtbGF0ZXInKTtcbiAgdHJ5IHtcbiAgICBhd2FpdCBleGVjKGNvbW1hbmRMaW5lLmpvaW4oJyAnKSk7XG5cbiAgICBjb25zdCBhc3NlbWJseSA9IGNyZWF0ZUFzc2VtYmx5KG91dGRpcik7XG5cbiAgICByZXR1cm4geyBhc3NlbWJseSwgbG9jazogYXdhaXQgd3JpdGVyTG9jay5jb252ZXJ0VG9SZWFkZXJMb2NrKCkgfTtcbiAgfSBjYXRjaCAoZSkge1xuICAgIGF3YWl0IHdyaXRlckxvY2sucmVsZWFzZSgpO1xuICAgIHRocm93IGU7XG4gIH0gZmluYWxseSB7XG4gICAgYXdhaXQgY2xlYW51cFRlbXAoKTtcbiAgfVxuXG4gIGFzeW5jIGZ1bmN0aW9uIGV4ZWMoY29tbWFuZEFuZEFyZ3M6IHN0cmluZykge1xuICAgIHRyeSB7XG4gICAgICBhd2FpdCBuZXcgUHJvbWlzZTx2b2lkPigob2ssIGZhaWwpID0+IHtcbiAgICAgICAgLy8gV2UgdXNlIGEgc2xpZ2h0bHkgbG93ZXItbGV2ZWwgaW50ZXJmYWNlIHRvOlxuICAgICAgICAvL1xuICAgICAgICAvLyAtIFBhc3MgYXJndW1lbnRzIGluIGFuIGFycmF5IGluc3RlYWQgb2YgYSBzdHJpbmcsIHRvIGdldCBhcm91bmQgYVxuICAgICAgICAvLyAgIG51bWJlciBvZiBxdW90aW5nIGlzc3VlcyBpbnRyb2R1Y2VkIGJ5IHRoZSBpbnRlcm1lZGlhdGUgc2hlbGwgbGF5ZXJcbiAgICAgICAgLy8gICAod2hpY2ggd291bGQgYmUgZGlmZmVyZW50IGJldHdlZW4gTGludXggYW5kIFdpbmRvd3MpLlxuICAgICAgICAvL1xuICAgICAgICAvLyAtIEluaGVyaXQgc3RkZXJyIGZyb20gY29udHJvbGxpbmcgdGVybWluYWwuIFdlIGRvbid0IHVzZSB0aGUgY2FwdHVyZWQgdmFsdWVcbiAgICAgICAgLy8gICBhbnl3YXksIGFuZCBpZiB0aGUgc3VicHJvY2VzcyBpcyBwcmludGluZyB0byBpdCBmb3IgZGVidWdnaW5nIHB1cnBvc2VzIHRoZVxuICAgICAgICAvLyAgIHVzZXIgZ2V0cyB0byBzZWUgaXQgc29vbmVyLiBQbHVzLCBjYXB0dXJpbmcgZG9lc24ndCBpbnRlcmFjdCBuaWNlbHkgd2l0aCBzb21lXG4gICAgICAgIC8vICAgcHJvY2Vzc2VzIGxpa2UgTWF2ZW4uXG4gICAgICAgIGNvbnN0IHByb2MgPSBjaGlsZFByb2Nlc3Muc3Bhd24oY29tbWFuZEFuZEFyZ3MsIHtcbiAgICAgICAgICBzdGRpbzogWydpZ25vcmUnLCAnaW5oZXJpdCcsICdpbmhlcml0J10sXG4gICAgICAgICAgZGV0YWNoZWQ6IGZhbHNlLFxuICAgICAgICAgIHNoZWxsOiB0cnVlLFxuICAgICAgICAgIGVudjoge1xuICAgICAgICAgICAgLi4ucHJvY2Vzcy5lbnYsXG4gICAgICAgICAgICAuLi5lbnYsXG4gICAgICAgICAgfSxcbiAgICAgICAgfSk7XG5cbiAgICAgICAgcHJvYy5vbignZXJyb3InLCBmYWlsKTtcblxuICAgICAgICBwcm9jLm9uKCdleGl0JywgY29kZSA9PiB7XG4gICAgICAgICAgaWYgKGNvZGUgPT09IDApIHtcbiAgICAgICAgICAgIHJldHVybiBvaygpO1xuICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICByZXR1cm4gZmFpbChuZXcgVG9vbGtpdEVycm9yKGAke2NvbW1hbmRBbmRBcmdzfTogU3VicHJvY2VzcyBleGl0ZWQgd2l0aCBlcnJvciAke2NvZGV9YCkpO1xuICAgICAgICAgIH1cbiAgICAgICAgfSk7XG4gICAgICB9KTtcbiAgICB9IGNhdGNoIChlOiBhbnkpIHtcbiAgICAgIGF3YWl0IGRlYnVnRm4oYGZhaWxlZCBjb21tYW5kOiAke2NvbW1hbmRBbmRBcmdzfWApO1xuICAgICAgdGhyb3cgZTtcbiAgICB9XG4gIH1cbn1cblxuLyoqXG4gKiBDcmVhdGVzIGFuIGFzc2VtYmx5IHdpdGggZXJyb3IgaGFuZGxpbmdcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNyZWF0ZUFzc2VtYmx5KGFwcERpcjogc3RyaW5nKSB7XG4gIHRyeSB7XG4gICAgcmV0dXJuIG5ldyBjeGFwaS5DbG91ZEFzc2VtYmx5KGFwcERpciwge1xuICAgICAgLy8gV2Ugc29ydCBhcyB3ZSBkZXBsb3lcbiAgICAgIHRvcG9Tb3J0OiBmYWxzZSxcbiAgICB9KTtcbiAgfSBjYXRjaCAoZXJyb3I6IGFueSkge1xuICAgIGlmIChlcnJvci5tZXNzYWdlLmluY2x1ZGVzKGN4c2NoZW1hLlZFUlNJT05fTUlTTUFUQ0gpKSB7XG4gICAgICAvLyB0aGlzIG1lYW5zIHRoZSBDTEkgdmVyc2lvbiBpcyB0b28gb2xkLlxuICAgICAgLy8gd2UgaW5zdHJ1Y3QgdGhlIHVzZXIgdG8gdXBncmFkZS5cbiAgICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoYFRoaXMgQ0RLIENMSSBpcyBub3QgY29tcGF0aWJsZSB3aXRoIHRoZSBDREsgbGlicmFyeSB1c2VkIGJ5IHlvdXIgYXBwbGljYXRpb24uIFBsZWFzZSB1cGdyYWRlIHRoZSBDTEkgdG8gdGhlIGxhdGVzdCB2ZXJzaW9uLlxcbigke2Vycm9yLm1lc3NhZ2V9KWApO1xuICAgIH1cbiAgICB0aHJvdyBlcnJvcjtcbiAgfVxufVxuXG5mdW5jdGlvbiBub1VuZGVmaW5lZDxBPih4czogUmVjb3JkPHN0cmluZywgQT4pOiBSZWNvcmQ8c3RyaW5nLCBOb25OdWxsYWJsZTxBPj4ge1xuICByZXR1cm4gT2JqZWN0LmZyb21FbnRyaWVzKE9iamVjdC5lbnRyaWVzKHhzKS5maWx0ZXIoKFtfLCB2XSkgPT4gdiAhPT0gdW5kZWZpbmVkKSkgYXMgYW55O1xufVxuIl19