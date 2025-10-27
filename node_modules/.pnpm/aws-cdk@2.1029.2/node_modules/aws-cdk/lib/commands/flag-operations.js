"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.handleFlags = handleFlags;
exports.displayFlags = displayFlags;
const path = require("path");
const cloudformation_diff_1 = require("@aws-cdk/cloudformation-diff");
const toolkit_lib_1 = require("@aws-cdk/toolkit-lib");
const chalk = require("chalk");
// @ts-ignore
const enquirer_1 = require("enquirer");
const fs = require("fs-extra");
const api_1 = require("../api");
const obsolete_flags_1 = require("../obsolete-flags");
var FlagsMenuOptions;
(function (FlagsMenuOptions) {
    FlagsMenuOptions["ALL_TO_RECOMMENDED"] = "Set all flags to recommended values";
    FlagsMenuOptions["UNCONFIGURED_TO_RECOMMENDED"] = "Set unconfigured flags to recommended values";
    FlagsMenuOptions["UNCONFIGURED_TO_DEFAULT"] = "Set unconfigured flags to their implied configuration (record current behavior)";
    FlagsMenuOptions["MODIFY_SPECIFIC_FLAG"] = "Modify a specific flag";
    FlagsMenuOptions["EXIT"] = "Exit";
})(FlagsMenuOptions || (FlagsMenuOptions = {}));
async function handleFlags(flagData, ioHelper, options, toolkit) {
    flagData = flagData.filter(flag => !obsolete_flags_1.OBSOLETE_FLAGS.includes(flag.name));
    if (flagData.length == 0) {
        await ioHelper.defaults.error('The \'cdk flags\' command is not compatible with the AWS CDK library used by your application. Please upgrade to 2.212.0 or above.');
        return;
    }
    let params = {
        flagData,
        toolkit,
        ioHelper,
        recommended: options.recommended,
        all: options.all,
        value: options.value,
        flagName: options.FLAGNAME,
        default: options.default,
        unconfigured: options.unconfigured,
    };
    const interactiveOptions = Object.values(FlagsMenuOptions);
    if (options.interactive) {
        const prompt = new enquirer_1.Select({
            name: 'option',
            message: 'Menu',
            choices: interactiveOptions,
        });
        const answer = await prompt.run();
        if (answer == FlagsMenuOptions.ALL_TO_RECOMMENDED) {
            params = {
                ...params,
                recommended: true,
                all: true,
            };
            await setMultipleFlags(params);
        }
        else if (answer == FlagsMenuOptions.UNCONFIGURED_TO_RECOMMENDED) {
            params = {
                ...params,
                recommended: true,
                unconfigured: true,
            };
            await setMultipleFlags(params);
        }
        else if (answer == FlagsMenuOptions.UNCONFIGURED_TO_DEFAULT) {
            params = {
                ...params,
                default: true,
                unconfigured: true,
            };
            await setMultipleFlagsIfSupported(params);
        }
        else if (answer == FlagsMenuOptions.MODIFY_SPECIFIC_FLAG) {
            await setFlag(params, true);
        }
        else if (answer == FlagsMenuOptions.EXIT) {
            return;
        }
        return;
    }
    if (options.FLAGNAME && options.all) {
        await ioHelper.defaults.error('Error: Cannot use both --all and a specific flag name. Please use either --all to show all flags or specify a single flag name.');
        return;
    }
    if ((options.value || options.recommended || options.default || options.unconfigured) && !options.set) {
        await ioHelper.defaults.error('Error: This option can only be used with --set.');
        return;
    }
    if (options.value && !options.FLAGNAME) {
        await ioHelper.defaults.error('Error: --value requires a specific flag name. Please specify a flag name when providing a value.');
        return;
    }
    if (options.recommended && options.default) {
        await ioHelper.defaults.error('Error: Cannot use both --recommended and --default. Please choose one option.');
        return;
    }
    if (options.unconfigured && options.all) {
        await ioHelper.defaults.error('Error: Cannot use both --unconfigured and --all. Please choose one option.');
        return;
    }
    if (options.unconfigured && options.FLAGNAME) {
        await ioHelper.defaults.error('Error: Cannot use --unconfigured with a specific flag name. --unconfigured works with multiple flags.');
        return;
    }
    if (options.set && options.FLAGNAME && !options.value) {
        await ioHelper.defaults.error('Error: When setting a specific flag, you must provide a --value.');
        return;
    }
    if (options.set && options.all && !options.recommended && !options.default) {
        await ioHelper.defaults.error('Error: When using --set with --all, you must specify either --recommended or --default.');
        return;
    }
    if (options.set && options.unconfigured && !options.recommended && !options.default) {
        await ioHelper.defaults.error('Error: When using --set with --unconfigured, you must specify either --recommended or --default.');
        return;
    }
    if (options.set && !options.all && !options.unconfigured && !options.FLAGNAME) {
        await ioHelper.defaults.error('Error: When using --set, you must specify either --all, --unconfigured, or provide a specific flag name.');
        return;
    }
    if (options.FLAGNAME && !options.set && !options.value) {
        await displayFlags(params);
        return;
    }
    if (options.all && !options.set) {
        await displayFlags(params);
        return;
    }
    if (options.set && options.FLAGNAME && options.value) {
        await setFlag(params);
        return;
    }
    if (!options.FLAGNAME && !options.all && !options.set) {
        await displayFlags(params);
        return;
    }
    if (options.set && options.all && options.recommended) {
        await setMultipleFlags(params);
        return;
    }
    if (options.set && options.all && options.default) {
        await setMultipleFlagsIfSupported(params);
    }
    if (options.set && options.unconfigured && options.recommended) {
        await setMultipleFlags(params);
        return;
    }
    if (options.set && options.unconfigured && options.default) {
        await setMultipleFlagsIfSupported(params);
    }
}
/**
 * Sets flag configurations to default values if `unconfiguredBehavesLike` is populated
 */
async function setMultipleFlagsIfSupported(params) {
    const { flagData, ioHelper } = params;
    if (flagData[0].unconfiguredBehavesLike) {
        await setMultipleFlags(params);
        return;
    }
    await ioHelper.defaults.error('The --default options are not compatible with the AWS CDK library used by your application. Please upgrade to 2.212.0 or above.');
}
async function setFlag(params, interactive) {
    const { flagData, ioHelper, flagName } = params;
    let updatedParams = params;
    let updatedFlagName = flagName;
    if (interactive) {
        const allFlagNames = flagData.filter(flag => isBooleanFlag(flag) == true).map(flag => flag.name);
        const prompt = new enquirer_1.Select({
            name: 'flag',
            message: 'Select which flag you would like to modify:',
            limit: 100,
            choices: allFlagNames,
        });
        const selectedFlagName = await prompt.run();
        updatedFlagName = [selectedFlagName];
        const valuePrompt = new enquirer_1.Select({
            name: 'value',
            message: 'Select a value:',
            choices: ['true', 'false'],
        });
        const updatedValue = await valuePrompt.run();
        updatedParams = {
            ...params,
            value: updatedValue,
            flagName: updatedFlagName,
        };
    }
    else {
        const flag = flagData.find(f => f.name === flagName[0]);
        if (!flag) {
            await ioHelper.defaults.error('Flag not found.');
            return;
        }
        if (!isBooleanFlag(flag)) {
            await ioHelper.defaults.error(`Flag '${flagName}' is not a boolean flag. Only boolean flags are currently supported.`);
            return;
        }
    }
    const prototypeSuccess = await prototypeChanges(updatedParams, updatedFlagName);
    if (prototypeSuccess) {
        await handleUserResponse(updatedParams, updatedFlagName);
    }
}
async function prototypeChanges(params, flagNames) {
    const { flagData, toolkit, ioHelper, recommended, value } = params;
    const baseContext = new toolkit_lib_1.CdkAppMultiContext(process.cwd());
    const baseContextValues = await baseContext.read();
    const memoryContext = new toolkit_lib_1.MemoryContext(baseContextValues);
    const cdkJson = await JSON.parse(await fs.readFile(path.join(process.cwd(), 'cdk.json'), 'utf-8'));
    const app = cdkJson.app;
    const source = await toolkit.fromCdkApp(app, {
        contextStore: baseContext,
        outdir: path.join(process.cwd(), 'original'),
    });
    const updateObj = {};
    const boolValue = toBooleanValue(value);
    if (flagNames.length === 1 && value !== undefined) {
        const flagName = flagNames[0];
        if (baseContextValues[flagName] == boolValue) {
            await ioHelper.defaults.info('Flag is already set to the specified value. No changes needed.');
            return false;
        }
        updateObj[flagName] = boolValue;
    }
    else {
        for (const flagName of flagNames) {
            const flag = flagData.find(f => f.name === flagName);
            if (!flag) {
                await ioHelper.defaults.error(`Flag ${flagName} not found.`);
                return false;
            }
            const newValue = recommended
                ? toBooleanValue(flag.recommendedValue)
                : String(flag.unconfiguredBehavesLike?.v2) === 'true';
            updateObj[flagName] = newValue;
        }
    }
    await memoryContext.update(updateObj);
    const cx = await toolkit.synth(source);
    const assembly = cx.cloudAssembly;
    const modifiedSource = await toolkit.fromCdkApp(app, {
        contextStore: memoryContext,
        outdir: path.join(process.cwd(), 'temp'),
    });
    const modifiedCx = await toolkit.synth(modifiedSource);
    const allStacks = assembly.stacksRecursively;
    for (const stack of allStacks) {
        const templatePath = stack.templateFullPath;
        await toolkit.diff(modifiedCx, {
            method: toolkit_lib_1.DiffMethod.LocalFile(templatePath),
            stacks: {
                strategy: api_1.StackSelectionStrategy.PATTERN_MUST_MATCH_SINGLE,
                patterns: [stack.hierarchicalId],
            },
        });
    }
    return true;
}
async function setMultipleFlags(params) {
    const { flagData, all } = params;
    let flagsToSet;
    if (all) {
        flagsToSet = flagData.filter(flag => flag.userValue === undefined || !isUserValueEqualToRecommended(flag))
            .filter(flag => isBooleanFlag(flag))
            .map(flag => flag.name);
    }
    else {
        flagsToSet = flagData.filter(flag => flag.userValue === undefined)
            .filter(flag => isBooleanFlag(flag))
            .map(flag => flag.name);
    }
    const prototypeSuccess = await prototypeChanges(params, flagsToSet);
    if (prototypeSuccess) {
        await handleUserResponse(params, flagsToSet);
    }
}
async function handleUserResponse(params, flagNames) {
    const { ioHelper } = params;
    const userAccepted = await ioHelper.requestResponse({
        time: new Date(),
        level: 'info',
        code: 'CDK_TOOLKIT_I9300',
        message: 'Do you want to accept these changes?',
        data: {
            flagNames,
            responseDescription: 'Enter "y" to apply changes or "n" to cancel',
        },
        defaultResponse: false,
    });
    if (userAccepted) {
        await modifyValues(params, flagNames);
        await ioHelper.defaults.info('Flag value(s) updated successfully.');
    }
    else {
        await ioHelper.defaults.info('Operation cancelled');
    }
    const originalDir = path.join(process.cwd(), 'original');
    const tempDir = path.join(process.cwd(), 'temp');
    await fs.remove(originalDir);
    await fs.remove(tempDir);
}
async function modifyValues(params, flagNames) {
    const { flagData, ioHelper, value, recommended } = params;
    const cdkJsonPath = path.join(process.cwd(), 'cdk.json');
    const cdkJsonContent = await fs.readFile(cdkJsonPath, 'utf-8');
    const cdkJson = JSON.parse(cdkJsonContent);
    if (flagNames.length == 1) {
        const boolValue = toBooleanValue(value);
        cdkJson.context[String(flagNames[0])] = boolValue;
        await ioHelper.defaults.info(`Setting flag '${flagNames}' to: ${boolValue}`);
    }
    else {
        for (const flagName of flagNames) {
            const flag = flagData.find(f => f.name === flagName);
            const newValue = recommended
                ? toBooleanValue(flag.recommendedValue)
                : String(flag.unconfiguredBehavesLike?.v2) === 'true';
            cdkJson.context[flagName] = newValue;
        }
    }
    await fs.writeFile(cdkJsonPath, JSON.stringify(cdkJson, null, 2), 'utf-8');
}
function getFlagSortOrder(flag) {
    if (flag.userValue === undefined) {
        return 3;
    }
    else if (isUserValueEqualToRecommended(flag)) {
        return 1;
    }
    else {
        return 2;
    }
}
async function displayFlagTable(flags, ioHelper) {
    const filteredFlags = flags.filter(flag => flag.unconfiguredBehavesLike?.v2 !== flag.recommendedValue);
    const sortedFlags = [...filteredFlags].sort((a, b) => {
        const orderA = getFlagSortOrder(a);
        const orderB = getFlagSortOrder(b);
        if (orderA !== orderB) {
            return orderA - orderB;
        }
        if (a.module !== b.module) {
            return a.module.localeCompare(b.module);
        }
        return a.name.localeCompare(b.name);
    });
    const rows = [];
    rows.push(['Feature Flag Name', 'Recommended Value', 'User Value']);
    let currentModule = '';
    sortedFlags.forEach((flag) => {
        if (flag.module !== currentModule) {
            rows.push([chalk.bold(`Module: ${flag.module}`), '', '']);
            currentModule = flag.module;
        }
        rows.push([
            `  ${flag.name}`,
            String(flag.recommendedValue),
            flag.userValue === undefined ? '<unset>' : String(flag.userValue),
        ]);
    });
    const formattedTable = (0, cloudformation_diff_1.formatTable)(rows, undefined, true);
    await ioHelper.defaults.info(formattedTable);
}
async function displayFlags(params) {
    const { flagData, ioHelper, flagName, all } = params;
    if (flagName && flagName.length > 0) {
        const matchingFlags = flagData.filter(f => flagName.some(searchTerm => f.name.toLowerCase().includes(searchTerm.toLowerCase())));
        if (matchingFlags.length === 0) {
            await ioHelper.defaults.error(`Flag matching "${flagName.join(', ')}" not found.`);
            return;
        }
        if (matchingFlags.length === 1) {
            const flag = matchingFlags[0];
            await ioHelper.defaults.info(`Flag name: ${flag.name}`);
            await ioHelper.defaults.info(`Description: ${flag.explanation}`);
            await ioHelper.defaults.info(`Recommended value: ${flag.recommendedValue}`);
            await ioHelper.defaults.info(`User value: ${flag.userValue}`);
            return;
        }
        await ioHelper.defaults.info(`Found ${matchingFlags.length} flags matching "${flagName.join(', ')}":`);
        await displayFlagTable(matchingFlags, ioHelper);
        return;
    }
    let flagsToDisplay;
    if (all) {
        flagsToDisplay = flagData;
    }
    else {
        flagsToDisplay = flagData.filter(flag => flag.userValue === undefined || !isUserValueEqualToRecommended(flag));
    }
    await displayFlagTable(flagsToDisplay, ioHelper);
    // Add helpful message after empty table when not using --all
    if (!all && flagsToDisplay.length === 0) {
        await ioHelper.defaults.info('');
        await ioHelper.defaults.info('✅ All feature flags are already set to their recommended values.');
        await ioHelper.defaults.info('Use \'cdk flags --all --unstable=flags\' to see all flags and their current values.');
    }
}
function isUserValueEqualToRecommended(flag) {
    return String(flag.userValue) === String(flag.recommendedValue);
}
function toBooleanValue(value) {
    if (typeof value === 'boolean') {
        return value;
    }
    if (typeof value === 'string') {
        return value.toLowerCase() === 'true';
    }
    return false;
}
function isBooleanFlag(flag) {
    const recommended = flag.recommendedValue;
    return typeof recommended === 'boolean' ||
        recommended === 'true' ||
        recommended === 'false';
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZmxhZy1vcGVyYXRpb25zLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiZmxhZy1vcGVyYXRpb25zLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7O0FBNkNBLGtDQWtKQztBQTJQRCxvQ0E0Q0M7QUF0ZUQsNkJBQTZCO0FBQzdCLHNFQUEyRDtBQUUzRCxzREFBcUY7QUFDckYsK0JBQStCO0FBQy9CLGFBQWE7QUFDYix1Q0FBa0M7QUFDbEMsK0JBQStCO0FBQy9CLGdDQUFnRDtBQUdoRCxzREFBbUQ7QUFFbkQsSUFBSyxnQkFNSjtBQU5ELFdBQUssZ0JBQWdCO0lBQ25CLDhFQUEwRCxDQUFBO0lBQzFELGdHQUE0RSxDQUFBO0lBQzVFLCtIQUEyRyxDQUFBO0lBQzNHLG1FQUErQyxDQUFBO0lBQy9DLGlDQUFhLENBQUE7QUFDZixDQUFDLEVBTkksZ0JBQWdCLEtBQWhCLGdCQUFnQixRQU1wQjtBQTBCTSxLQUFLLFVBQVUsV0FBVyxDQUFDLFFBQXVCLEVBQUUsUUFBa0IsRUFBRSxPQUFxQixFQUFFLE9BQWdCO0lBQ3BILFFBQVEsR0FBRyxRQUFRLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsQ0FBQywrQkFBYyxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztJQUV4RSxJQUFJLFFBQVEsQ0FBQyxNQUFNLElBQUksQ0FBQyxFQUFFLENBQUM7UUFDekIsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxvSUFBb0ksQ0FBQyxDQUFDO1FBQ3BLLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxNQUFNLEdBQUc7UUFDWCxRQUFRO1FBQ1IsT0FBTztRQUNQLFFBQVE7UUFDUixXQUFXLEVBQUUsT0FBTyxDQUFDLFdBQVc7UUFDaEMsR0FBRyxFQUFFLE9BQU8sQ0FBQyxHQUFHO1FBQ2hCLEtBQUssRUFBRSxPQUFPLENBQUMsS0FBSztRQUNwQixRQUFRLEVBQUUsT0FBTyxDQUFDLFFBQVE7UUFDMUIsT0FBTyxFQUFFLE9BQU8sQ0FBQyxPQUFPO1FBQ3hCLFlBQVksRUFBRSxPQUFPLENBQUMsWUFBWTtLQUNuQyxDQUFDO0lBRUYsTUFBTSxrQkFBa0IsR0FBRyxNQUFNLENBQUMsTUFBTSxDQUFDLGdCQUFnQixDQUFDLENBQUM7SUFFM0QsSUFBSSxPQUFPLENBQUMsV0FBVyxFQUFFLENBQUM7UUFDeEIsTUFBTSxNQUFNLEdBQUcsSUFBSSxpQkFBTSxDQUFDO1lBQ3hCLElBQUksRUFBRSxRQUFRO1lBQ2QsT0FBTyxFQUFFLE1BQU07WUFDZixPQUFPLEVBQUUsa0JBQWtCO1NBQzVCLENBQUMsQ0FBQztRQUVILE1BQU0sTUFBTSxHQUFHLE1BQU0sTUFBTSxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQ2xDLElBQUksTUFBTSxJQUFJLGdCQUFnQixDQUFDLGtCQUFrQixFQUFFLENBQUM7WUFDbEQsTUFBTSxHQUFHO2dCQUNQLEdBQUcsTUFBTTtnQkFDVCxXQUFXLEVBQUUsSUFBSTtnQkFDakIsR0FBRyxFQUFFLElBQUk7YUFDVixDQUFDO1lBQ0YsTUFBTSxnQkFBZ0IsQ0FBQyxNQUFNLENBQUMsQ0FBQztRQUNqQyxDQUFDO2FBQU0sSUFBSSxNQUFNLElBQUksZ0JBQWdCLENBQUMsMkJBQTJCLEVBQUUsQ0FBQztZQUNsRSxNQUFNLEdBQUc7Z0JBQ1AsR0FBRyxNQUFNO2dCQUNULFdBQVcsRUFBRSxJQUFJO2dCQUNqQixZQUFZLEVBQUUsSUFBSTthQUNuQixDQUFDO1lBQ0YsTUFBTSxnQkFBZ0IsQ0FBQyxNQUFNLENBQUMsQ0FBQztRQUNqQyxDQUFDO2FBQU0sSUFBSSxNQUFNLElBQUksZ0JBQWdCLENBQUMsdUJBQXVCLEVBQUUsQ0FBQztZQUM5RCxNQUFNLEdBQUc7Z0JBQ1AsR0FBRyxNQUFNO2dCQUNULE9BQU8sRUFBRSxJQUFJO2dCQUNiLFlBQVksRUFBRSxJQUFJO2FBQ25CLENBQUM7WUFDRixNQUFNLDJCQUEyQixDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQzVDLENBQUM7YUFBTSxJQUFJLE1BQU0sSUFBSSxnQkFBZ0IsQ0FBQyxvQkFBb0IsRUFBRSxDQUFDO1lBQzNELE1BQU0sT0FBTyxDQUFDLE1BQU0sRUFBRSxJQUFJLENBQUMsQ0FBQztRQUM5QixDQUFDO2FBQU0sSUFBSSxNQUFNLElBQUksZ0JBQWdCLENBQUMsSUFBSSxFQUFFLENBQUM7WUFDM0MsT0FBTztRQUNULENBQUM7UUFDRCxPQUFPO0lBQ1QsQ0FBQztJQUVELElBQUksT0FBTyxDQUFDLFFBQVEsSUFBSSxPQUFPLENBQUMsR0FBRyxFQUFFLENBQUM7UUFDcEMsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxpSUFBaUksQ0FBQyxDQUFDO1FBQ2pLLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxDQUFDLE9BQU8sQ0FBQyxLQUFLLElBQUksT0FBTyxDQUFDLFdBQVcsSUFBSSxPQUFPLENBQUMsT0FBTyxJQUFJLE9BQU8sQ0FBQyxZQUFZLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxHQUFHLEVBQUUsQ0FBQztRQUN0RyxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsS0FBSyxDQUFDLGlEQUFpRCxDQUFDLENBQUM7UUFDakYsT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxLQUFLLElBQUksQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLENBQUM7UUFDdkMsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxrR0FBa0csQ0FBQyxDQUFDO1FBQ2xJLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsV0FBVyxJQUFJLE9BQU8sQ0FBQyxPQUFPLEVBQUUsQ0FBQztRQUMzQyxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsS0FBSyxDQUFDLCtFQUErRSxDQUFDLENBQUM7UUFDL0csT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxZQUFZLElBQUksT0FBTyxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQ3hDLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsNEVBQTRFLENBQUMsQ0FBQztRQUM1RyxPQUFPO0lBQ1QsQ0FBQztJQUVELElBQUksT0FBTyxDQUFDLFlBQVksSUFBSSxPQUFPLENBQUMsUUFBUSxFQUFFLENBQUM7UUFDN0MsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyx1R0FBdUcsQ0FBQyxDQUFDO1FBQ3ZJLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLE9BQU8sQ0FBQyxRQUFRLElBQUksQ0FBQyxPQUFPLENBQUMsS0FBSyxFQUFFLENBQUM7UUFDdEQsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxrRUFBa0UsQ0FBQyxDQUFDO1FBQ2xHLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLE9BQU8sQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsV0FBVyxJQUFJLENBQUMsT0FBTyxDQUFDLE9BQU8sRUFBRSxDQUFDO1FBQzNFLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMseUZBQXlGLENBQUMsQ0FBQztRQUN6SCxPQUFPO0lBQ1QsQ0FBQztJQUVELElBQUksT0FBTyxDQUFDLEdBQUcsSUFBSSxPQUFPLENBQUMsWUFBWSxJQUFJLENBQUMsT0FBTyxDQUFDLFdBQVcsSUFBSSxDQUFDLE9BQU8sQ0FBQyxPQUFPLEVBQUUsQ0FBQztRQUNwRixNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsS0FBSyxDQUFDLGtHQUFrRyxDQUFDLENBQUM7UUFDbEksT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxHQUFHLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLFlBQVksSUFBSSxDQUFDLE9BQU8sQ0FBQyxRQUFRLEVBQUUsQ0FBQztRQUM5RSxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsS0FBSyxDQUFDLDBHQUEwRyxDQUFDLENBQUM7UUFDMUksT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxRQUFRLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLEtBQUssRUFBRSxDQUFDO1FBQ3ZELE1BQU0sWUFBWSxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQzNCLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQ2hDLE1BQU0sWUFBWSxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQzNCLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLE9BQU8sQ0FBQyxRQUFRLElBQUksT0FBTyxDQUFDLEtBQUssRUFBRSxDQUFDO1FBQ3JELE1BQU0sT0FBTyxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQ3RCLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxDQUFDLE9BQU8sQ0FBQyxRQUFRLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQ3RELE1BQU0sWUFBWSxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQzNCLE9BQU87SUFDVCxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLE9BQU8sQ0FBQyxHQUFHLElBQUksT0FBTyxDQUFDLFdBQVcsRUFBRSxDQUFDO1FBQ3RELE1BQU0sZ0JBQWdCLENBQUMsTUFBTSxDQUFDLENBQUM7UUFDL0IsT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxHQUFHLElBQUksT0FBTyxDQUFDLEdBQUcsSUFBSSxPQUFPLENBQUMsT0FBTyxFQUFFLENBQUM7UUFDbEQsTUFBTSwyQkFBMkIsQ0FBQyxNQUFNLENBQUMsQ0FBQztJQUM1QyxDQUFDO0lBRUQsSUFBSSxPQUFPLENBQUMsR0FBRyxJQUFJLE9BQU8sQ0FBQyxZQUFZLElBQUksT0FBTyxDQUFDLFdBQVcsRUFBRSxDQUFDO1FBQy9ELE1BQU0sZ0JBQWdCLENBQUMsTUFBTSxDQUFDLENBQUM7UUFDL0IsT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLE9BQU8sQ0FBQyxHQUFHLElBQUksT0FBTyxDQUFDLFlBQVksSUFBSSxPQUFPLENBQUMsT0FBTyxFQUFFLENBQUM7UUFDM0QsTUFBTSwyQkFBMkIsQ0FBQyxNQUFNLENBQUMsQ0FBQztJQUM1QyxDQUFDO0FBQ0gsQ0FBQztBQUVEOztHQUVHO0FBQ0gsS0FBSyxVQUFVLDJCQUEyQixDQUFDLE1BQTRCO0lBQ3JFLE1BQU0sRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFFLEdBQUcsTUFBTSxDQUFDO0lBQ3RDLElBQUksUUFBUSxDQUFDLENBQUMsQ0FBQyxDQUFDLHVCQUF1QixFQUFFLENBQUM7UUFDeEMsTUFBTSxnQkFBZ0IsQ0FBQyxNQUFNLENBQUMsQ0FBQztRQUMvQixPQUFPO0lBQ1QsQ0FBQztJQUNELE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsaUlBQWlJLENBQUMsQ0FBQztBQUNuSyxDQUFDO0FBRUQsS0FBSyxVQUFVLE9BQU8sQ0FBQyxNQUE0QixFQUFFLFdBQXFCO0lBQ3hFLE1BQU0sRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFFLFFBQVEsRUFBRSxHQUFHLE1BQU0sQ0FBQztJQUNoRCxJQUFJLGFBQWEsR0FBRyxNQUFNLENBQUM7SUFDM0IsSUFBSSxlQUFlLEdBQUcsUUFBUSxDQUFDO0lBRS9CLElBQUksV0FBVyxFQUFFLENBQUM7UUFDaEIsTUFBTSxZQUFZLEdBQUcsUUFBUSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsSUFBSSxJQUFJLENBQUMsQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDLEVBQUUsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7UUFFakcsTUFBTSxNQUFNLEdBQUcsSUFBSSxpQkFBTSxDQUFDO1lBQ3hCLElBQUksRUFBRSxNQUFNO1lBQ1osT0FBTyxFQUFFLDZDQUE2QztZQUN0RCxLQUFLLEVBQUUsR0FBRztZQUNWLE9BQU8sRUFBRSxZQUFZO1NBQ3RCLENBQUMsQ0FBQztRQUVILE1BQU0sZ0JBQWdCLEdBQUcsTUFBTSxNQUFNLENBQUMsR0FBRyxFQUFFLENBQUM7UUFDNUMsZUFBZSxHQUFHLENBQUMsZ0JBQWdCLENBQUMsQ0FBQztRQUVyQyxNQUFNLFdBQVcsR0FBRyxJQUFJLGlCQUFNLENBQUM7WUFDN0IsSUFBSSxFQUFFLE9BQU87WUFDYixPQUFPLEVBQUUsaUJBQWlCO1lBQzFCLE9BQU8sRUFBRSxDQUFDLE1BQU0sRUFBRSxPQUFPLENBQUM7U0FDM0IsQ0FBQyxDQUFDO1FBRUgsTUFBTSxZQUFZLEdBQUcsTUFBTSxXQUFXLENBQUMsR0FBRyxFQUFFLENBQUM7UUFFN0MsYUFBYSxHQUFHO1lBQ2QsR0FBRyxNQUFNO1lBQ1QsS0FBSyxFQUFFLFlBQVk7WUFDbkIsUUFBUSxFQUFFLGVBQWU7U0FDMUIsQ0FBQztJQUNKLENBQUM7U0FBTSxDQUFDO1FBQ04sTUFBTSxJQUFJLEdBQUcsUUFBUSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLEtBQUssUUFBUyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFFekQsSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO1lBQ1YsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDO1lBQ2pELE9BQU87UUFDVCxDQUFDO1FBRUQsSUFBSSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDO1lBQ3pCLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsU0FBUyxRQUFRLHNFQUFzRSxDQUFDLENBQUM7WUFDdkgsT0FBTztRQUNULENBQUM7SUFDSCxDQUFDO0lBRUQsTUFBTSxnQkFBZ0IsR0FBRyxNQUFNLGdCQUFnQixDQUFDLGFBQWEsRUFBRSxlQUFnQixDQUFDLENBQUM7SUFFakYsSUFBSSxnQkFBZ0IsRUFBRSxDQUFDO1FBQ3JCLE1BQU0sa0JBQWtCLENBQUMsYUFBYSxFQUFFLGVBQWdCLENBQUMsQ0FBQztJQUM1RCxDQUFDO0FBQ0gsQ0FBQztBQUVELEtBQUssVUFBVSxnQkFBZ0IsQ0FDN0IsTUFBNEIsRUFDNUIsU0FBbUI7SUFFbkIsTUFBTSxFQUFFLFFBQVEsRUFBRSxPQUFPLEVBQUUsUUFBUSxFQUFFLFdBQVcsRUFBRSxLQUFLLEVBQUUsR0FBRyxNQUFNLENBQUM7SUFDbkUsTUFBTSxXQUFXLEdBQUcsSUFBSSxnQ0FBa0IsQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLENBQUMsQ0FBQztJQUMxRCxNQUFNLGlCQUFpQixHQUFHLE1BQU0sV0FBVyxDQUFDLElBQUksRUFBRSxDQUFDO0lBQ25ELE1BQU0sYUFBYSxHQUFHLElBQUksMkJBQWEsQ0FBQyxpQkFBaUIsQ0FBQyxDQUFDO0lBRTNELE1BQU0sT0FBTyxHQUFHLE1BQU0sSUFBSSxDQUFDLEtBQUssQ0FBQyxNQUFNLEVBQUUsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLEVBQUUsVUFBVSxDQUFDLEVBQUUsT0FBTyxDQUFDLENBQUMsQ0FBQztJQUNuRyxNQUFNLEdBQUcsR0FBRyxPQUFPLENBQUMsR0FBRyxDQUFDO0lBRXhCLE1BQU0sTUFBTSxHQUFHLE1BQU0sT0FBTyxDQUFDLFVBQVUsQ0FBQyxHQUFHLEVBQUU7UUFDM0MsWUFBWSxFQUFFLFdBQVc7UUFDekIsTUFBTSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxFQUFFLFVBQVUsQ0FBQztLQUM3QyxDQUFDLENBQUM7SUFFSCxNQUFNLFNBQVMsR0FBNEIsRUFBRSxDQUFDO0lBQzlDLE1BQU0sU0FBUyxHQUFHLGNBQWMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUN4QyxJQUFJLFNBQVMsQ0FBQyxNQUFNLEtBQUssQ0FBQyxJQUFJLEtBQUssS0FBSyxTQUFTLEVBQUUsQ0FBQztRQUNsRCxNQUFNLFFBQVEsR0FBRyxTQUFTLENBQUMsQ0FBQyxDQUFDLENBQUM7UUFDOUIsSUFBSSxpQkFBaUIsQ0FBQyxRQUFRLENBQUMsSUFBSSxTQUFTLEVBQUUsQ0FBQztZQUM3QyxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLGdFQUFnRSxDQUFDLENBQUM7WUFDL0YsT0FBTyxLQUFLLENBQUM7UUFDZixDQUFDO1FBQ0QsU0FBUyxDQUFDLFFBQVEsQ0FBQyxHQUFHLFNBQVMsQ0FBQztJQUNsQyxDQUFDO1NBQU0sQ0FBQztRQUNOLEtBQUssTUFBTSxRQUFRLElBQUksU0FBUyxFQUFFLENBQUM7WUFDakMsTUFBTSxJQUFJLEdBQUcsUUFBUSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLEtBQUssUUFBUSxDQUFDLENBQUM7WUFDckQsSUFBSSxDQUFDLElBQUksRUFBRSxDQUFDO2dCQUNWLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsUUFBUSxRQUFRLGFBQWEsQ0FBQyxDQUFDO2dCQUM3RCxPQUFPLEtBQUssQ0FBQztZQUNmLENBQUM7WUFDRCxNQUFNLFFBQVEsR0FBRyxXQUFXO2dCQUMxQixDQUFDLENBQUMsY0FBYyxDQUFDLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQztnQkFDdkMsQ0FBQyxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsdUJBQXVCLEVBQUUsRUFBRSxDQUFDLEtBQUssTUFBTSxDQUFDO1lBQ3hELFNBQVMsQ0FBQyxRQUFRLENBQUMsR0FBRyxRQUFRLENBQUM7UUFDakMsQ0FBQztJQUNILENBQUM7SUFFRCxNQUFNLGFBQWEsQ0FBQyxNQUFNLENBQUMsU0FBUyxDQUFDLENBQUM7SUFDdEMsTUFBTSxFQUFFLEdBQUcsTUFBTSxPQUFPLENBQUMsS0FBSyxDQUFDLE1BQU0sQ0FBQyxDQUFDO0lBQ3ZDLE1BQU0sUUFBUSxHQUFHLEVBQUUsQ0FBQyxhQUFhLENBQUM7SUFFbEMsTUFBTSxjQUFjLEdBQUcsTUFBTSxPQUFPLENBQUMsVUFBVSxDQUFDLEdBQUcsRUFBRTtRQUNuRCxZQUFZLEVBQUUsYUFBYTtRQUMzQixNQUFNLEVBQUUsSUFBSSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLEVBQUUsTUFBTSxDQUFDO0tBQ3pDLENBQUMsQ0FBQztJQUVILE1BQU0sVUFBVSxHQUFHLE1BQU0sT0FBTyxDQUFDLEtBQUssQ0FBQyxjQUFjLENBQUMsQ0FBQztJQUN2RCxNQUFNLFNBQVMsR0FBRyxRQUFRLENBQUMsaUJBQWlCLENBQUM7SUFFN0MsS0FBSyxNQUFNLEtBQUssSUFBSSxTQUFTLEVBQUUsQ0FBQztRQUM5QixNQUFNLFlBQVksR0FBRyxLQUFLLENBQUMsZ0JBQWdCLENBQUM7UUFDNUMsTUFBTSxPQUFPLENBQUMsSUFBSSxDQUFDLFVBQVUsRUFBRTtZQUM3QixNQUFNLEVBQUUsd0JBQVUsQ0FBQyxTQUFTLENBQUMsWUFBWSxDQUFDO1lBQzFDLE1BQU0sRUFBRTtnQkFDTixRQUFRLEVBQUUsNEJBQXNCLENBQUMseUJBQXlCO2dCQUMxRCxRQUFRLEVBQUUsQ0FBQyxLQUFLLENBQUMsY0FBYyxDQUFDO2FBQ2pDO1NBQ0YsQ0FBQyxDQUFDO0lBQ0wsQ0FBQztJQUNELE9BQU8sSUFBSSxDQUFDO0FBQ2QsQ0FBQztBQUVELEtBQUssVUFBVSxnQkFBZ0IsQ0FBQyxNQUE0QjtJQUMxRCxNQUFNLEVBQUUsUUFBUSxFQUFFLEdBQUcsRUFBRSxHQUFHLE1BQU0sQ0FBQztJQUNqQyxJQUFJLFVBQVUsQ0FBQztJQUNmLElBQUksR0FBRyxFQUFFLENBQUM7UUFDUixVQUFVLEdBQUcsUUFBUSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyxTQUFTLEtBQUssU0FBUyxJQUFJLENBQUMsNkJBQTZCLENBQUMsSUFBSSxDQUFDLENBQUM7YUFDdkcsTUFBTSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsYUFBYSxDQUFDLElBQUksQ0FBQyxDQUFDO2FBQ25DLEdBQUcsQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUM1QixDQUFDO1NBQU0sQ0FBQztRQUNOLFVBQVUsR0FBRyxRQUFRLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQ2xDLElBQUksQ0FBQyxTQUFTLEtBQUssU0FBUyxDQUFDO2FBQzVCLE1BQU0sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLGFBQWEsQ0FBQyxJQUFJLENBQUMsQ0FBQzthQUNuQyxHQUFHLENBQUMsSUFBSSxDQUFDLEVBQUUsQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7SUFDNUIsQ0FBQztJQUNELE1BQU0sZ0JBQWdCLEdBQUcsTUFBTSxnQkFBZ0IsQ0FBQyxNQUFNLEVBQUUsVUFBVSxDQUFDLENBQUM7SUFFcEUsSUFBSSxnQkFBZ0IsRUFBRSxDQUFDO1FBQ3JCLE1BQU0sa0JBQWtCLENBQUMsTUFBTSxFQUFFLFVBQVUsQ0FBQyxDQUFDO0lBQy9DLENBQUM7QUFDSCxDQUFDO0FBRUQsS0FBSyxVQUFVLGtCQUFrQixDQUMvQixNQUE0QixFQUM1QixTQUFtQjtJQUVuQixNQUFNLEVBQUUsUUFBUSxFQUFFLEdBQUcsTUFBTSxDQUFDO0lBQzVCLE1BQU0sWUFBWSxHQUFHLE1BQU0sUUFBUSxDQUFDLGVBQWUsQ0FBQztRQUNsRCxJQUFJLEVBQUUsSUFBSSxJQUFJLEVBQUU7UUFDaEIsS0FBSyxFQUFFLE1BQU07UUFDYixJQUFJLEVBQUUsbUJBQW1CO1FBQ3pCLE9BQU8sRUFBRSxzQ0FBc0M7UUFDL0MsSUFBSSxFQUFFO1lBQ0osU0FBUztZQUNULG1CQUFtQixFQUFFLDZDQUE2QztTQUNuRTtRQUNELGVBQWUsRUFBRSxLQUFLO0tBQ3ZCLENBQUMsQ0FBQztJQUNILElBQUksWUFBWSxFQUFFLENBQUM7UUFDakIsTUFBTSxZQUFZLENBQUMsTUFBTSxFQUFFLFNBQVMsQ0FBQyxDQUFDO1FBQ3RDLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMscUNBQXFDLENBQUMsQ0FBQztJQUN0RSxDQUFDO1NBQU0sQ0FBQztRQUNOLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMscUJBQXFCLENBQUMsQ0FBQztJQUN0RCxDQUFDO0lBRUQsTUFBTSxXQUFXLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLEVBQUUsVUFBVSxDQUFDLENBQUM7SUFDekQsTUFBTSxPQUFPLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLEVBQUUsTUFBTSxDQUFDLENBQUM7SUFFakQsTUFBTSxFQUFFLENBQUMsTUFBTSxDQUFDLFdBQVcsQ0FBQyxDQUFDO0lBQzdCLE1BQU0sRUFBRSxDQUFDLE1BQU0sQ0FBQyxPQUFPLENBQUMsQ0FBQztBQUMzQixDQUFDO0FBRUQsS0FBSyxVQUFVLFlBQVksQ0FBQyxNQUE0QixFQUFFLFNBQW1CO0lBQzNFLE1BQU0sRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFFLEtBQUssRUFBRSxXQUFXLEVBQUUsR0FBRyxNQUFNLENBQUM7SUFDMUQsTUFBTSxXQUFXLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLEVBQUUsVUFBVSxDQUFDLENBQUM7SUFDekQsTUFBTSxjQUFjLEdBQUcsTUFBTSxFQUFFLENBQUMsUUFBUSxDQUFDLFdBQVcsRUFBRSxPQUFPLENBQUMsQ0FBQztJQUMvRCxNQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsS0FBSyxDQUFDLGNBQWMsQ0FBQyxDQUFDO0lBRTNDLElBQUksU0FBUyxDQUFDLE1BQU0sSUFBSSxDQUFDLEVBQUUsQ0FBQztRQUMxQixNQUFNLFNBQVMsR0FBRyxjQUFjLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDeEMsT0FBTyxDQUFDLE9BQU8sQ0FBQyxNQUFNLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsR0FBRyxTQUFTLENBQUM7UUFFbEQsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxpQkFBaUIsU0FBUyxTQUFTLFNBQVMsRUFBRSxDQUFDLENBQUM7SUFDL0UsQ0FBQztTQUFNLENBQUM7UUFDTixLQUFLLE1BQU0sUUFBUSxJQUFJLFNBQVMsRUFBRSxDQUFDO1lBQ2pDLE1BQU0sSUFBSSxHQUFHLFFBQVEsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUMsSUFBSSxLQUFLLFFBQVEsQ0FBQyxDQUFDO1lBQ3JELE1BQU0sUUFBUSxHQUFHLFdBQVc7Z0JBQzFCLENBQUMsQ0FBQyxjQUFjLENBQUMsSUFBSyxDQUFDLGdCQUFnQixDQUFDO2dCQUN4QyxDQUFDLENBQUMsTUFBTSxDQUFDLElBQUssQ0FBQyx1QkFBdUIsRUFBRSxFQUFFLENBQUMsS0FBSyxNQUFNLENBQUM7WUFDekQsT0FBTyxDQUFDLE9BQU8sQ0FBQyxRQUFRLENBQUMsR0FBRyxRQUFRLENBQUM7UUFDdkMsQ0FBQztJQUNILENBQUM7SUFFRCxNQUFNLEVBQUUsQ0FBQyxTQUFTLENBQUMsV0FBVyxFQUFFLElBQUksQ0FBQyxTQUFTLENBQUMsT0FBTyxFQUFFLElBQUksRUFBRSxDQUFDLENBQUMsRUFBRSxPQUFPLENBQUMsQ0FBQztBQUM3RSxDQUFDO0FBRUQsU0FBUyxnQkFBZ0IsQ0FBQyxJQUFpQjtJQUN6QyxJQUFJLElBQUksQ0FBQyxTQUFTLEtBQUssU0FBUyxFQUFFLENBQUM7UUFDakMsT0FBTyxDQUFDLENBQUM7SUFDWCxDQUFDO1NBQU0sSUFBSSw2QkFBNkIsQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDO1FBQy9DLE9BQU8sQ0FBQyxDQUFDO0lBQ1gsQ0FBQztTQUFNLENBQUM7UUFDTixPQUFPLENBQUMsQ0FBQztJQUNYLENBQUM7QUFDSCxDQUFDO0FBRUQsS0FBSyxVQUFVLGdCQUFnQixDQUFDLEtBQW9CLEVBQUUsUUFBa0I7SUFDdEUsTUFBTSxhQUFhLEdBQUcsS0FBSyxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyx1QkFBdUIsRUFBRSxFQUFFLEtBQUssSUFBSSxDQUFDLGdCQUFnQixDQUFDLENBQUM7SUFFdkcsTUFBTSxXQUFXLEdBQUcsQ0FBQyxHQUFHLGFBQWEsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLEVBQUUsRUFBRTtRQUNuRCxNQUFNLE1BQU0sR0FBRyxnQkFBZ0IsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUNuQyxNQUFNLE1BQU0sR0FBRyxnQkFBZ0IsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUVuQyxJQUFJLE1BQU0sS0FBSyxNQUFNLEVBQUUsQ0FBQztZQUN0QixPQUFPLE1BQU0sR0FBRyxNQUFNLENBQUM7UUFDekIsQ0FBQztRQUNELElBQUksQ0FBQyxDQUFDLE1BQU0sS0FBSyxDQUFDLENBQUMsTUFBTSxFQUFFLENBQUM7WUFDMUIsT0FBTyxDQUFDLENBQUMsTUFBTSxDQUFDLGFBQWEsQ0FBQyxDQUFDLENBQUMsTUFBTSxDQUFDLENBQUM7UUFDMUMsQ0FBQztRQUNELE9BQU8sQ0FBQyxDQUFDLElBQUksQ0FBQyxhQUFhLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ3RDLENBQUMsQ0FBQyxDQUFDO0lBRUgsTUFBTSxJQUFJLEdBQWUsRUFBRSxDQUFDO0lBQzVCLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxtQkFBbUIsRUFBRSxtQkFBbUIsRUFBRSxZQUFZLENBQUMsQ0FBQyxDQUFDO0lBQ3BFLElBQUksYUFBYSxHQUFHLEVBQUUsQ0FBQztJQUV2QixXQUFXLENBQUMsT0FBTyxDQUFDLENBQUMsSUFBSSxFQUFFLEVBQUU7UUFDM0IsSUFBSSxJQUFJLENBQUMsTUFBTSxLQUFLLGFBQWEsRUFBRSxDQUFDO1lBQ2xDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQyxLQUFLLENBQUMsSUFBSSxDQUFDLFdBQVcsSUFBSSxDQUFDLE1BQU0sRUFBRSxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsQ0FBQyxDQUFDLENBQUM7WUFDMUQsYUFBYSxHQUFHLElBQUksQ0FBQyxNQUFNLENBQUM7UUFDOUIsQ0FBQztRQUNELElBQUksQ0FBQyxJQUFJLENBQUM7WUFDUixLQUFLLElBQUksQ0FBQyxJQUFJLEVBQUU7WUFDaEIsTUFBTSxDQUFDLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQztZQUM3QixJQUFJLENBQUMsU0FBUyxLQUFLLFNBQVMsQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLFNBQVMsQ0FBQztTQUNsRSxDQUFDLENBQUM7SUFDTCxDQUFDLENBQUMsQ0FBQztJQUVILE1BQU0sY0FBYyxHQUFHLElBQUEsaUNBQVcsRUFBQyxJQUFJLEVBQUUsU0FBUyxFQUFFLElBQUksQ0FBQyxDQUFDO0lBQzFELE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsY0FBYyxDQUFDLENBQUM7QUFDL0MsQ0FBQztBQUVNLEtBQUssVUFBVSxZQUFZLENBQUMsTUFBNEI7SUFDN0QsTUFBTSxFQUFFLFFBQVEsRUFBRSxRQUFRLEVBQUUsUUFBUSxFQUFFLEdBQUcsRUFBRSxHQUFHLE1BQU0sQ0FBQztJQUVyRCxJQUFJLFFBQVEsSUFBSSxRQUFRLENBQUMsTUFBTSxHQUFHLENBQUMsRUFBRSxDQUFDO1FBQ3BDLE1BQU0sYUFBYSxHQUFHLFFBQVEsQ0FBQyxNQUFNLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FDeEMsUUFBUSxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsUUFBUSxDQUFDLFVBQVUsQ0FBQyxXQUFXLEVBQUUsQ0FBQyxDQUFDLENBQ3JGLENBQUM7UUFFRixJQUFJLGFBQWEsQ0FBQyxNQUFNLEtBQUssQ0FBQyxFQUFFLENBQUM7WUFDL0IsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxrQkFBa0IsUUFBUSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsY0FBYyxDQUFDLENBQUM7WUFDbkYsT0FBTztRQUNULENBQUM7UUFFRCxJQUFJLGFBQWEsQ0FBQyxNQUFNLEtBQUssQ0FBQyxFQUFFLENBQUM7WUFDL0IsTUFBTSxJQUFJLEdBQUcsYUFBYSxDQUFDLENBQUMsQ0FBQyxDQUFDO1lBQzlCLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsY0FBYyxJQUFJLENBQUMsSUFBSSxFQUFFLENBQUMsQ0FBQztZQUN4RCxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLGdCQUFnQixJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQztZQUNqRSxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLHNCQUFzQixJQUFJLENBQUMsZ0JBQWdCLEVBQUUsQ0FBQyxDQUFDO1lBQzVFLE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsZUFBZSxJQUFJLENBQUMsU0FBUyxFQUFFLENBQUMsQ0FBQztZQUM5RCxPQUFPO1FBQ1QsQ0FBQztRQUVELE1BQU0sUUFBUSxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsU0FBUyxhQUFhLENBQUMsTUFBTSxvQkFBb0IsUUFBUSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLENBQUM7UUFDdkcsTUFBTSxnQkFBZ0IsQ0FBQyxhQUFhLEVBQUUsUUFBUSxDQUFDLENBQUM7UUFDaEQsT0FBTztJQUNULENBQUM7SUFFRCxJQUFJLGNBQTZCLENBQUM7SUFDbEMsSUFBSSxHQUFHLEVBQUUsQ0FBQztRQUNSLGNBQWMsR0FBRyxRQUFRLENBQUM7SUFDNUIsQ0FBQztTQUFNLENBQUM7UUFDTixjQUFjLEdBQUcsUUFBUSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsRUFBRSxDQUN0QyxJQUFJLENBQUMsU0FBUyxLQUFLLFNBQVMsSUFBSSxDQUFDLDZCQUE2QixDQUFDLElBQUksQ0FBQyxDQUNyRSxDQUFDO0lBQ0osQ0FBQztJQUVELE1BQU0sZ0JBQWdCLENBQUMsY0FBYyxFQUFFLFFBQVEsQ0FBQyxDQUFDO0lBRWpELDZEQUE2RDtJQUM3RCxJQUFJLENBQUMsR0FBRyxJQUFJLGNBQWMsQ0FBQyxNQUFNLEtBQUssQ0FBQyxFQUFFLENBQUM7UUFDeEMsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsQ0FBQztRQUNqQyxNQUFNLFFBQVEsQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLGtFQUFrRSxDQUFDLENBQUM7UUFDakcsTUFBTSxRQUFRLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxxRkFBcUYsQ0FBQyxDQUFDO0lBQ3RILENBQUM7QUFDSCxDQUFDO0FBRUQsU0FBUyw2QkFBNkIsQ0FBQyxJQUFpQjtJQUN0RCxPQUFPLE1BQU0sQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDLEtBQUssTUFBTSxDQUFDLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQyxDQUFDO0FBQ2xFLENBQUM7QUFFRCxTQUFTLGNBQWMsQ0FBQyxLQUFjO0lBQ3BDLElBQUksT0FBTyxLQUFLLEtBQUssU0FBUyxFQUFFLENBQUM7UUFDL0IsT0FBTyxLQUFLLENBQUM7SUFDZixDQUFDO0lBQ0QsSUFBSSxPQUFPLEtBQUssS0FBSyxRQUFRLEVBQUUsQ0FBQztRQUM5QixPQUFPLEtBQUssQ0FBQyxXQUFXLEVBQUUsS0FBSyxNQUFNLENBQUM7SUFDeEMsQ0FBQztJQUNELE9BQU8sS0FBSyxDQUFDO0FBQ2YsQ0FBQztBQUVELFNBQVMsYUFBYSxDQUFDLElBQWlCO0lBQ3RDLE1BQU0sV0FBVyxHQUFHLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQztJQUMxQyxPQUFPLE9BQU8sV0FBVyxLQUFLLFNBQVM7UUFDckMsV0FBVyxLQUFLLE1BQU07UUFDdEIsV0FBVyxLQUFLLE9BQU8sQ0FBQztBQUM1QixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0ICogYXMgcGF0aCBmcm9tICdwYXRoJztcbmltcG9ydCB7IGZvcm1hdFRhYmxlIH0gZnJvbSAnQGF3cy1jZGsvY2xvdWRmb3JtYXRpb24tZGlmZic7XG5pbXBvcnQgdHlwZSB7IEZlYXR1cmVGbGFnLCBUb29sa2l0IH0gZnJvbSAnQGF3cy1jZGsvdG9vbGtpdC1saWInO1xuaW1wb3J0IHsgQ2RrQXBwTXVsdGlDb250ZXh0LCBNZW1vcnlDb250ZXh0LCBEaWZmTWV0aG9kIH0gZnJvbSAnQGF3cy1jZGsvdG9vbGtpdC1saWInO1xuaW1wb3J0ICogYXMgY2hhbGsgZnJvbSAnY2hhbGsnO1xuLy8gQHRzLWlnbm9yZVxuaW1wb3J0IHsgU2VsZWN0IH0gZnJvbSAnZW5xdWlyZXInO1xuaW1wb3J0ICogYXMgZnMgZnJvbSAnZnMtZXh0cmEnO1xuaW1wb3J0IHsgU3RhY2tTZWxlY3Rpb25TdHJhdGVneSB9IGZyb20gJy4uL2FwaSc7XG5pbXBvcnQgdHlwZSB7IElvSGVscGVyIH0gZnJvbSAnLi4vYXBpLXByaXZhdGUnO1xuaW1wb3J0IHR5cGUgeyBGbGFnc09wdGlvbnMgfSBmcm9tICcuLi9jbGkvdXNlci1pbnB1dCc7XG5pbXBvcnQgeyBPQlNPTEVURV9GTEFHUyB9IGZyb20gJy4uL29ic29sZXRlLWZsYWdzJztcblxuZW51bSBGbGFnc01lbnVPcHRpb25zIHtcbiAgQUxMX1RPX1JFQ09NTUVOREVEID0gJ1NldCBhbGwgZmxhZ3MgdG8gcmVjb21tZW5kZWQgdmFsdWVzJyxcbiAgVU5DT05GSUdVUkVEX1RPX1JFQ09NTUVOREVEID0gJ1NldCB1bmNvbmZpZ3VyZWQgZmxhZ3MgdG8gcmVjb21tZW5kZWQgdmFsdWVzJyxcbiAgVU5DT05GSUdVUkVEX1RPX0RFRkFVTFQgPSAnU2V0IHVuY29uZmlndXJlZCBmbGFncyB0byB0aGVpciBpbXBsaWVkIGNvbmZpZ3VyYXRpb24gKHJlY29yZCBjdXJyZW50IGJlaGF2aW9yKScsXG4gIE1PRElGWV9TUEVDSUZJQ19GTEFHID0gJ01vZGlmeSBhIHNwZWNpZmljIGZsYWcnLFxuICBFWElUID0gJ0V4aXQnLFxufVxuXG5pbnRlcmZhY2UgRmxhZ09wZXJhdGlvbnNQYXJhbXMge1xuICBmbGFnRGF0YTogRmVhdHVyZUZsYWdbXTtcbiAgdG9vbGtpdDogVG9vbGtpdDtcbiAgaW9IZWxwZXI6IElvSGVscGVyO1xuXG4gIC8qKiBVc2VyIHJhbiAtLXJlY29tbWVuZGVkIG9wdGlvbiAqL1xuICByZWNvbW1lbmRlZD86IGJvb2xlYW47XG5cbiAgLyoqIFVzZXIgcmFuIC0tYWxsIG9wdGlvbiAqL1xuICBhbGw/OiBib29sZWFuO1xuXG4gIC8qKiBVc2VyIHByb3ZpZGVkIC0tdmFsdWUgZmllbGQgKi9cbiAgdmFsdWU/OiBzdHJpbmc7XG5cbiAgLyoqIFVzZXIgcHJvdmlkZWQgRkxBR05BTUUgZmllbGQgKi9cbiAgZmxhZ05hbWU/OiBzdHJpbmdbXTtcblxuICAvKiogVXNlciByYW4gLS1kZWZhdWx0IG9wdGlvbiAqL1xuICBkZWZhdWx0PzogYm9vbGVhbjtcblxuICAvKiogVXNlciByYW4gLS11bmNvbmZpZ3VyZWQgb3B0aW9uICovXG4gIHVuY29uZmlndXJlZD86IGJvb2xlYW47XG59XG5cbmV4cG9ydCBhc3luYyBmdW5jdGlvbiBoYW5kbGVGbGFncyhmbGFnRGF0YTogRmVhdHVyZUZsYWdbXSwgaW9IZWxwZXI6IElvSGVscGVyLCBvcHRpb25zOiBGbGFnc09wdGlvbnMsIHRvb2xraXQ6IFRvb2xraXQpIHtcbiAgZmxhZ0RhdGEgPSBmbGFnRGF0YS5maWx0ZXIoZmxhZyA9PiAhT0JTT0xFVEVfRkxBR1MuaW5jbHVkZXMoZmxhZy5uYW1lKSk7XG5cbiAgaWYgKGZsYWdEYXRhLmxlbmd0aCA9PSAwKSB7XG4gICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuZXJyb3IoJ1RoZSBcXCdjZGsgZmxhZ3NcXCcgY29tbWFuZCBpcyBub3QgY29tcGF0aWJsZSB3aXRoIHRoZSBBV1MgQ0RLIGxpYnJhcnkgdXNlZCBieSB5b3VyIGFwcGxpY2F0aW9uLiBQbGVhc2UgdXBncmFkZSB0byAyLjIxMi4wIG9yIGFib3ZlLicpO1xuICAgIHJldHVybjtcbiAgfVxuXG4gIGxldCBwYXJhbXMgPSB7XG4gICAgZmxhZ0RhdGEsXG4gICAgdG9vbGtpdCxcbiAgICBpb0hlbHBlcixcbiAgICByZWNvbW1lbmRlZDogb3B0aW9ucy5yZWNvbW1lbmRlZCxcbiAgICBhbGw6IG9wdGlvbnMuYWxsLFxuICAgIHZhbHVlOiBvcHRpb25zLnZhbHVlLFxuICAgIGZsYWdOYW1lOiBvcHRpb25zLkZMQUdOQU1FLFxuICAgIGRlZmF1bHQ6IG9wdGlvbnMuZGVmYXVsdCxcbiAgICB1bmNvbmZpZ3VyZWQ6IG9wdGlvbnMudW5jb25maWd1cmVkLFxuICB9O1xuXG4gIGNvbnN0IGludGVyYWN0aXZlT3B0aW9ucyA9IE9iamVjdC52YWx1ZXMoRmxhZ3NNZW51T3B0aW9ucyk7XG5cbiAgaWYgKG9wdGlvbnMuaW50ZXJhY3RpdmUpIHtcbiAgICBjb25zdCBwcm9tcHQgPSBuZXcgU2VsZWN0KHtcbiAgICAgIG5hbWU6ICdvcHRpb24nLFxuICAgICAgbWVzc2FnZTogJ01lbnUnLFxuICAgICAgY2hvaWNlczogaW50ZXJhY3RpdmVPcHRpb25zLFxuICAgIH0pO1xuXG4gICAgY29uc3QgYW5zd2VyID0gYXdhaXQgcHJvbXB0LnJ1bigpO1xuICAgIGlmIChhbnN3ZXIgPT0gRmxhZ3NNZW51T3B0aW9ucy5BTExfVE9fUkVDT01NRU5ERUQpIHtcbiAgICAgIHBhcmFtcyA9IHtcbiAgICAgICAgLi4ucGFyYW1zLFxuICAgICAgICByZWNvbW1lbmRlZDogdHJ1ZSxcbiAgICAgICAgYWxsOiB0cnVlLFxuICAgICAgfTtcbiAgICAgIGF3YWl0IHNldE11bHRpcGxlRmxhZ3MocGFyYW1zKTtcbiAgICB9IGVsc2UgaWYgKGFuc3dlciA9PSBGbGFnc01lbnVPcHRpb25zLlVOQ09ORklHVVJFRF9UT19SRUNPTU1FTkRFRCkge1xuICAgICAgcGFyYW1zID0ge1xuICAgICAgICAuLi5wYXJhbXMsXG4gICAgICAgIHJlY29tbWVuZGVkOiB0cnVlLFxuICAgICAgICB1bmNvbmZpZ3VyZWQ6IHRydWUsXG4gICAgICB9O1xuICAgICAgYXdhaXQgc2V0TXVsdGlwbGVGbGFncyhwYXJhbXMpO1xuICAgIH0gZWxzZSBpZiAoYW5zd2VyID09IEZsYWdzTWVudU9wdGlvbnMuVU5DT05GSUdVUkVEX1RPX0RFRkFVTFQpIHtcbiAgICAgIHBhcmFtcyA9IHtcbiAgICAgICAgLi4ucGFyYW1zLFxuICAgICAgICBkZWZhdWx0OiB0cnVlLFxuICAgICAgICB1bmNvbmZpZ3VyZWQ6IHRydWUsXG4gICAgICB9O1xuICAgICAgYXdhaXQgc2V0TXVsdGlwbGVGbGFnc0lmU3VwcG9ydGVkKHBhcmFtcyk7XG4gICAgfSBlbHNlIGlmIChhbnN3ZXIgPT0gRmxhZ3NNZW51T3B0aW9ucy5NT0RJRllfU1BFQ0lGSUNfRkxBRykge1xuICAgICAgYXdhaXQgc2V0RmxhZyhwYXJhbXMsIHRydWUpO1xuICAgIH0gZWxzZSBpZiAoYW5zd2VyID09IEZsYWdzTWVudU9wdGlvbnMuRVhJVCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5GTEFHTkFNRSAmJiBvcHRpb25zLmFsbCkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogQ2Fubm90IHVzZSBib3RoIC0tYWxsIGFuZCBhIHNwZWNpZmljIGZsYWcgbmFtZS4gUGxlYXNlIHVzZSBlaXRoZXIgLS1hbGwgdG8gc2hvdyBhbGwgZmxhZ3Mgb3Igc3BlY2lmeSBhIHNpbmdsZSBmbGFnIG5hbWUuJyk7XG4gICAgcmV0dXJuO1xuICB9XG5cbiAgaWYgKChvcHRpb25zLnZhbHVlIHx8IG9wdGlvbnMucmVjb21tZW5kZWQgfHwgb3B0aW9ucy5kZWZhdWx0IHx8IG9wdGlvbnMudW5jb25maWd1cmVkKSAmJiAhb3B0aW9ucy5zZXQpIHtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcignRXJyb3I6IFRoaXMgb3B0aW9uIGNhbiBvbmx5IGJlIHVzZWQgd2l0aCAtLXNldC4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy52YWx1ZSAmJiAhb3B0aW9ucy5GTEFHTkFNRSkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogLS12YWx1ZSByZXF1aXJlcyBhIHNwZWNpZmljIGZsYWcgbmFtZS4gUGxlYXNlIHNwZWNpZnkgYSBmbGFnIG5hbWUgd2hlbiBwcm92aWRpbmcgYSB2YWx1ZS4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5yZWNvbW1lbmRlZCAmJiBvcHRpb25zLmRlZmF1bHQpIHtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcignRXJyb3I6IENhbm5vdCB1c2UgYm90aCAtLXJlY29tbWVuZGVkIGFuZCAtLWRlZmF1bHQuIFBsZWFzZSBjaG9vc2Ugb25lIG9wdGlvbi4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy51bmNvbmZpZ3VyZWQgJiYgb3B0aW9ucy5hbGwpIHtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcignRXJyb3I6IENhbm5vdCB1c2UgYm90aCAtLXVuY29uZmlndXJlZCBhbmQgLS1hbGwuIFBsZWFzZSBjaG9vc2Ugb25lIG9wdGlvbi4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy51bmNvbmZpZ3VyZWQgJiYgb3B0aW9ucy5GTEFHTkFNRSkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogQ2Fubm90IHVzZSAtLXVuY29uZmlndXJlZCB3aXRoIGEgc3BlY2lmaWMgZmxhZyBuYW1lLiAtLXVuY29uZmlndXJlZCB3b3JrcyB3aXRoIG11bHRpcGxlIGZsYWdzLicpO1xuICAgIHJldHVybjtcbiAgfVxuXG4gIGlmIChvcHRpb25zLnNldCAmJiBvcHRpb25zLkZMQUdOQU1FICYmICFvcHRpb25zLnZhbHVlKSB7XG4gICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuZXJyb3IoJ0Vycm9yOiBXaGVuIHNldHRpbmcgYSBzcGVjaWZpYyBmbGFnLCB5b3UgbXVzdCBwcm92aWRlIGEgLS12YWx1ZS4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5zZXQgJiYgb3B0aW9ucy5hbGwgJiYgIW9wdGlvbnMucmVjb21tZW5kZWQgJiYgIW9wdGlvbnMuZGVmYXVsdCkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogV2hlbiB1c2luZyAtLXNldCB3aXRoIC0tYWxsLCB5b3UgbXVzdCBzcGVjaWZ5IGVpdGhlciAtLXJlY29tbWVuZGVkIG9yIC0tZGVmYXVsdC4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5zZXQgJiYgb3B0aW9ucy51bmNvbmZpZ3VyZWQgJiYgIW9wdGlvbnMucmVjb21tZW5kZWQgJiYgIW9wdGlvbnMuZGVmYXVsdCkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogV2hlbiB1c2luZyAtLXNldCB3aXRoIC0tdW5jb25maWd1cmVkLCB5b3UgbXVzdCBzcGVjaWZ5IGVpdGhlciAtLXJlY29tbWVuZGVkIG9yIC0tZGVmYXVsdC4nKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5zZXQgJiYgIW9wdGlvbnMuYWxsICYmICFvcHRpb25zLnVuY29uZmlndXJlZCAmJiAhb3B0aW9ucy5GTEFHTkFNRSkge1xuICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmVycm9yKCdFcnJvcjogV2hlbiB1c2luZyAtLXNldCwgeW91IG11c3Qgc3BlY2lmeSBlaXRoZXIgLS1hbGwsIC0tdW5jb25maWd1cmVkLCBvciBwcm92aWRlIGEgc3BlY2lmaWMgZmxhZyBuYW1lLicpO1xuICAgIHJldHVybjtcbiAgfVxuXG4gIGlmIChvcHRpb25zLkZMQUdOQU1FICYmICFvcHRpb25zLnNldCAmJiAhb3B0aW9ucy52YWx1ZSkge1xuICAgIGF3YWl0IGRpc3BsYXlGbGFncyhwYXJhbXMpO1xuICAgIHJldHVybjtcbiAgfVxuXG4gIGlmIChvcHRpb25zLmFsbCAmJiAhb3B0aW9ucy5zZXQpIHtcbiAgICBhd2FpdCBkaXNwbGF5RmxhZ3MocGFyYW1zKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBpZiAob3B0aW9ucy5zZXQgJiYgb3B0aW9ucy5GTEFHTkFNRSAmJiBvcHRpb25zLnZhbHVlKSB7XG4gICAgYXdhaXQgc2V0RmxhZyhwYXJhbXMpO1xuICAgIHJldHVybjtcbiAgfVxuXG4gIGlmICghb3B0aW9ucy5GTEFHTkFNRSAmJiAhb3B0aW9ucy5hbGwgJiYgIW9wdGlvbnMuc2V0KSB7XG4gICAgYXdhaXQgZGlzcGxheUZsYWdzKHBhcmFtcyk7XG4gICAgcmV0dXJuO1xuICB9XG5cbiAgaWYgKG9wdGlvbnMuc2V0ICYmIG9wdGlvbnMuYWxsICYmIG9wdGlvbnMucmVjb21tZW5kZWQpIHtcbiAgICBhd2FpdCBzZXRNdWx0aXBsZUZsYWdzKHBhcmFtcyk7XG4gICAgcmV0dXJuO1xuICB9XG5cbiAgaWYgKG9wdGlvbnMuc2V0ICYmIG9wdGlvbnMuYWxsICYmIG9wdGlvbnMuZGVmYXVsdCkge1xuICAgIGF3YWl0IHNldE11bHRpcGxlRmxhZ3NJZlN1cHBvcnRlZChwYXJhbXMpO1xuICB9XG5cbiAgaWYgKG9wdGlvbnMuc2V0ICYmIG9wdGlvbnMudW5jb25maWd1cmVkICYmIG9wdGlvbnMucmVjb21tZW5kZWQpIHtcbiAgICBhd2FpdCBzZXRNdWx0aXBsZUZsYWdzKHBhcmFtcyk7XG4gICAgcmV0dXJuO1xuICB9XG5cbiAgaWYgKG9wdGlvbnMuc2V0ICYmIG9wdGlvbnMudW5jb25maWd1cmVkICYmIG9wdGlvbnMuZGVmYXVsdCkge1xuICAgIGF3YWl0IHNldE11bHRpcGxlRmxhZ3NJZlN1cHBvcnRlZChwYXJhbXMpO1xuICB9XG59XG5cbi8qKlxuICogU2V0cyBmbGFnIGNvbmZpZ3VyYXRpb25zIHRvIGRlZmF1bHQgdmFsdWVzIGlmIGB1bmNvbmZpZ3VyZWRCZWhhdmVzTGlrZWAgaXMgcG9wdWxhdGVkXG4gKi9cbmFzeW5jIGZ1bmN0aW9uIHNldE11bHRpcGxlRmxhZ3NJZlN1cHBvcnRlZChwYXJhbXM6IEZsYWdPcGVyYXRpb25zUGFyYW1zKSB7XG4gIGNvbnN0IHsgZmxhZ0RhdGEsIGlvSGVscGVyIH0gPSBwYXJhbXM7XG4gIGlmIChmbGFnRGF0YVswXS51bmNvbmZpZ3VyZWRCZWhhdmVzTGlrZSkge1xuICAgIGF3YWl0IHNldE11bHRpcGxlRmxhZ3MocGFyYW1zKTtcbiAgICByZXR1cm47XG4gIH1cbiAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuZXJyb3IoJ1RoZSAtLWRlZmF1bHQgb3B0aW9ucyBhcmUgbm90IGNvbXBhdGlibGUgd2l0aCB0aGUgQVdTIENESyBsaWJyYXJ5IHVzZWQgYnkgeW91ciBhcHBsaWNhdGlvbi4gUGxlYXNlIHVwZ3JhZGUgdG8gMi4yMTIuMCBvciBhYm92ZS4nKTtcbn1cblxuYXN5bmMgZnVuY3Rpb24gc2V0RmxhZyhwYXJhbXM6IEZsYWdPcGVyYXRpb25zUGFyYW1zLCBpbnRlcmFjdGl2ZT86IGJvb2xlYW4pIHtcbiAgY29uc3QgeyBmbGFnRGF0YSwgaW9IZWxwZXIsIGZsYWdOYW1lIH0gPSBwYXJhbXM7XG4gIGxldCB1cGRhdGVkUGFyYW1zID0gcGFyYW1zO1xuICBsZXQgdXBkYXRlZEZsYWdOYW1lID0gZmxhZ05hbWU7XG5cbiAgaWYgKGludGVyYWN0aXZlKSB7XG4gICAgY29uc3QgYWxsRmxhZ05hbWVzID0gZmxhZ0RhdGEuZmlsdGVyKGZsYWcgPT4gaXNCb29sZWFuRmxhZyhmbGFnKSA9PSB0cnVlKS5tYXAoZmxhZyA9PiBmbGFnLm5hbWUpO1xuXG4gICAgY29uc3QgcHJvbXB0ID0gbmV3IFNlbGVjdCh7XG4gICAgICBuYW1lOiAnZmxhZycsXG4gICAgICBtZXNzYWdlOiAnU2VsZWN0IHdoaWNoIGZsYWcgeW91IHdvdWxkIGxpa2UgdG8gbW9kaWZ5OicsXG4gICAgICBsaW1pdDogMTAwLFxuICAgICAgY2hvaWNlczogYWxsRmxhZ05hbWVzLFxuICAgIH0pO1xuXG4gICAgY29uc3Qgc2VsZWN0ZWRGbGFnTmFtZSA9IGF3YWl0IHByb21wdC5ydW4oKTtcbiAgICB1cGRhdGVkRmxhZ05hbWUgPSBbc2VsZWN0ZWRGbGFnTmFtZV07XG5cbiAgICBjb25zdCB2YWx1ZVByb21wdCA9IG5ldyBTZWxlY3Qoe1xuICAgICAgbmFtZTogJ3ZhbHVlJyxcbiAgICAgIG1lc3NhZ2U6ICdTZWxlY3QgYSB2YWx1ZTonLFxuICAgICAgY2hvaWNlczogWyd0cnVlJywgJ2ZhbHNlJ10sXG4gICAgfSk7XG5cbiAgICBjb25zdCB1cGRhdGVkVmFsdWUgPSBhd2FpdCB2YWx1ZVByb21wdC5ydW4oKTtcblxuICAgIHVwZGF0ZWRQYXJhbXMgPSB7XG4gICAgICAuLi5wYXJhbXMsXG4gICAgICB2YWx1ZTogdXBkYXRlZFZhbHVlLFxuICAgICAgZmxhZ05hbWU6IHVwZGF0ZWRGbGFnTmFtZSxcbiAgICB9O1xuICB9IGVsc2Uge1xuICAgIGNvbnN0IGZsYWcgPSBmbGFnRGF0YS5maW5kKGYgPT4gZi5uYW1lID09PSBmbGFnTmFtZSFbMF0pO1xuXG4gICAgaWYgKCFmbGFnKSB7XG4gICAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcignRmxhZyBub3QgZm91bmQuJyk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKCFpc0Jvb2xlYW5GbGFnKGZsYWcpKSB7XG4gICAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcihgRmxhZyAnJHtmbGFnTmFtZX0nIGlzIG5vdCBhIGJvb2xlYW4gZmxhZy4gT25seSBib29sZWFuIGZsYWdzIGFyZSBjdXJyZW50bHkgc3VwcG9ydGVkLmApO1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgfVxuXG4gIGNvbnN0IHByb3RvdHlwZVN1Y2Nlc3MgPSBhd2FpdCBwcm90b3R5cGVDaGFuZ2VzKHVwZGF0ZWRQYXJhbXMsIHVwZGF0ZWRGbGFnTmFtZSEpO1xuXG4gIGlmIChwcm90b3R5cGVTdWNjZXNzKSB7XG4gICAgYXdhaXQgaGFuZGxlVXNlclJlc3BvbnNlKHVwZGF0ZWRQYXJhbXMsIHVwZGF0ZWRGbGFnTmFtZSEpO1xuICB9XG59XG5cbmFzeW5jIGZ1bmN0aW9uIHByb3RvdHlwZUNoYW5nZXMoXG4gIHBhcmFtczogRmxhZ09wZXJhdGlvbnNQYXJhbXMsXG4gIGZsYWdOYW1lczogc3RyaW5nW10sXG4pOiBQcm9taXNlPGJvb2xlYW4+IHtcbiAgY29uc3QgeyBmbGFnRGF0YSwgdG9vbGtpdCwgaW9IZWxwZXIsIHJlY29tbWVuZGVkLCB2YWx1ZSB9ID0gcGFyYW1zO1xuICBjb25zdCBiYXNlQ29udGV4dCA9IG5ldyBDZGtBcHBNdWx0aUNvbnRleHQocHJvY2Vzcy5jd2QoKSk7XG4gIGNvbnN0IGJhc2VDb250ZXh0VmFsdWVzID0gYXdhaXQgYmFzZUNvbnRleHQucmVhZCgpO1xuICBjb25zdCBtZW1vcnlDb250ZXh0ID0gbmV3IE1lbW9yeUNvbnRleHQoYmFzZUNvbnRleHRWYWx1ZXMpO1xuXG4gIGNvbnN0IGNka0pzb24gPSBhd2FpdCBKU09OLnBhcnNlKGF3YWl0IGZzLnJlYWRGaWxlKHBhdGguam9pbihwcm9jZXNzLmN3ZCgpLCAnY2RrLmpzb24nKSwgJ3V0Zi04JykpO1xuICBjb25zdCBhcHAgPSBjZGtKc29uLmFwcDtcblxuICBjb25zdCBzb3VyY2UgPSBhd2FpdCB0b29sa2l0LmZyb21DZGtBcHAoYXBwLCB7XG4gICAgY29udGV4dFN0b3JlOiBiYXNlQ29udGV4dCxcbiAgICBvdXRkaXI6IHBhdGguam9pbihwcm9jZXNzLmN3ZCgpLCAnb3JpZ2luYWwnKSxcbiAgfSk7XG5cbiAgY29uc3QgdXBkYXRlT2JqOiBSZWNvcmQ8c3RyaW5nLCBib29sZWFuPiA9IHt9O1xuICBjb25zdCBib29sVmFsdWUgPSB0b0Jvb2xlYW5WYWx1ZSh2YWx1ZSk7XG4gIGlmIChmbGFnTmFtZXMubGVuZ3RoID09PSAxICYmIHZhbHVlICE9PSB1bmRlZmluZWQpIHtcbiAgICBjb25zdCBmbGFnTmFtZSA9IGZsYWdOYW1lc1swXTtcbiAgICBpZiAoYmFzZUNvbnRleHRWYWx1ZXNbZmxhZ05hbWVdID09IGJvb2xWYWx1ZSkge1xuICAgICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuaW5mbygnRmxhZyBpcyBhbHJlYWR5IHNldCB0byB0aGUgc3BlY2lmaWVkIHZhbHVlLiBObyBjaGFuZ2VzIG5lZWRlZC4nKTtcbiAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9XG4gICAgdXBkYXRlT2JqW2ZsYWdOYW1lXSA9IGJvb2xWYWx1ZTtcbiAgfSBlbHNlIHtcbiAgICBmb3IgKGNvbnN0IGZsYWdOYW1lIG9mIGZsYWdOYW1lcykge1xuICAgICAgY29uc3QgZmxhZyA9IGZsYWdEYXRhLmZpbmQoZiA9PiBmLm5hbWUgPT09IGZsYWdOYW1lKTtcbiAgICAgIGlmICghZmxhZykge1xuICAgICAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcihgRmxhZyAke2ZsYWdOYW1lfSBub3QgZm91bmQuYCk7XG4gICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgIH1cbiAgICAgIGNvbnN0IG5ld1ZhbHVlID0gcmVjb21tZW5kZWRcbiAgICAgICAgPyB0b0Jvb2xlYW5WYWx1ZShmbGFnLnJlY29tbWVuZGVkVmFsdWUpXG4gICAgICAgIDogU3RyaW5nKGZsYWcudW5jb25maWd1cmVkQmVoYXZlc0xpa2U/LnYyKSA9PT0gJ3RydWUnO1xuICAgICAgdXBkYXRlT2JqW2ZsYWdOYW1lXSA9IG5ld1ZhbHVlO1xuICAgIH1cbiAgfVxuXG4gIGF3YWl0IG1lbW9yeUNvbnRleHQudXBkYXRlKHVwZGF0ZU9iaik7XG4gIGNvbnN0IGN4ID0gYXdhaXQgdG9vbGtpdC5zeW50aChzb3VyY2UpO1xuICBjb25zdCBhc3NlbWJseSA9IGN4LmNsb3VkQXNzZW1ibHk7XG5cbiAgY29uc3QgbW9kaWZpZWRTb3VyY2UgPSBhd2FpdCB0b29sa2l0LmZyb21DZGtBcHAoYXBwLCB7XG4gICAgY29udGV4dFN0b3JlOiBtZW1vcnlDb250ZXh0LFxuICAgIG91dGRpcjogcGF0aC5qb2luKHByb2Nlc3MuY3dkKCksICd0ZW1wJyksXG4gIH0pO1xuXG4gIGNvbnN0IG1vZGlmaWVkQ3ggPSBhd2FpdCB0b29sa2l0LnN5bnRoKG1vZGlmaWVkU291cmNlKTtcbiAgY29uc3QgYWxsU3RhY2tzID0gYXNzZW1ibHkuc3RhY2tzUmVjdXJzaXZlbHk7XG5cbiAgZm9yIChjb25zdCBzdGFjayBvZiBhbGxTdGFja3MpIHtcbiAgICBjb25zdCB0ZW1wbGF0ZVBhdGggPSBzdGFjay50ZW1wbGF0ZUZ1bGxQYXRoO1xuICAgIGF3YWl0IHRvb2xraXQuZGlmZihtb2RpZmllZEN4LCB7XG4gICAgICBtZXRob2Q6IERpZmZNZXRob2QuTG9jYWxGaWxlKHRlbXBsYXRlUGF0aCksXG4gICAgICBzdGFja3M6IHtcbiAgICAgICAgc3RyYXRlZ3k6IFN0YWNrU2VsZWN0aW9uU3RyYXRlZ3kuUEFUVEVSTl9NVVNUX01BVENIX1NJTkdMRSxcbiAgICAgICAgcGF0dGVybnM6IFtzdGFjay5oaWVyYXJjaGljYWxJZF0sXG4gICAgICB9LFxuICAgIH0pO1xuICB9XG4gIHJldHVybiB0cnVlO1xufVxuXG5hc3luYyBmdW5jdGlvbiBzZXRNdWx0aXBsZUZsYWdzKHBhcmFtczogRmxhZ09wZXJhdGlvbnNQYXJhbXMpIHtcbiAgY29uc3QgeyBmbGFnRGF0YSwgYWxsIH0gPSBwYXJhbXM7XG4gIGxldCBmbGFnc1RvU2V0O1xuICBpZiAoYWxsKSB7XG4gICAgZmxhZ3NUb1NldCA9IGZsYWdEYXRhLmZpbHRlcihmbGFnID0+IGZsYWcudXNlclZhbHVlID09PSB1bmRlZmluZWQgfHwgIWlzVXNlclZhbHVlRXF1YWxUb1JlY29tbWVuZGVkKGZsYWcpKVxuICAgICAgLmZpbHRlcihmbGFnID0+IGlzQm9vbGVhbkZsYWcoZmxhZykpXG4gICAgICAubWFwKGZsYWcgPT4gZmxhZy5uYW1lKTtcbiAgfSBlbHNlIHtcbiAgICBmbGFnc1RvU2V0ID0gZmxhZ0RhdGEuZmlsdGVyKGZsYWcgPT5cbiAgICAgIGZsYWcudXNlclZhbHVlID09PSB1bmRlZmluZWQpXG4gICAgICAuZmlsdGVyKGZsYWcgPT4gaXNCb29sZWFuRmxhZyhmbGFnKSlcbiAgICAgIC5tYXAoZmxhZyA9PiBmbGFnLm5hbWUpO1xuICB9XG4gIGNvbnN0IHByb3RvdHlwZVN1Y2Nlc3MgPSBhd2FpdCBwcm90b3R5cGVDaGFuZ2VzKHBhcmFtcywgZmxhZ3NUb1NldCk7XG5cbiAgaWYgKHByb3RvdHlwZVN1Y2Nlc3MpIHtcbiAgICBhd2FpdCBoYW5kbGVVc2VyUmVzcG9uc2UocGFyYW1zLCBmbGFnc1RvU2V0KTtcbiAgfVxufVxuXG5hc3luYyBmdW5jdGlvbiBoYW5kbGVVc2VyUmVzcG9uc2UoXG4gIHBhcmFtczogRmxhZ09wZXJhdGlvbnNQYXJhbXMsXG4gIGZsYWdOYW1lczogc3RyaW5nW10sXG4pOiBQcm9taXNlPHZvaWQ+IHtcbiAgY29uc3QgeyBpb0hlbHBlciB9ID0gcGFyYW1zO1xuICBjb25zdCB1c2VyQWNjZXB0ZWQgPSBhd2FpdCBpb0hlbHBlci5yZXF1ZXN0UmVzcG9uc2Uoe1xuICAgIHRpbWU6IG5ldyBEYXRlKCksXG4gICAgbGV2ZWw6ICdpbmZvJyxcbiAgICBjb2RlOiAnQ0RLX1RPT0xLSVRfSTkzMDAnLFxuICAgIG1lc3NhZ2U6ICdEbyB5b3Ugd2FudCB0byBhY2NlcHQgdGhlc2UgY2hhbmdlcz8nLFxuICAgIGRhdGE6IHtcbiAgICAgIGZsYWdOYW1lcyxcbiAgICAgIHJlc3BvbnNlRGVzY3JpcHRpb246ICdFbnRlciBcInlcIiB0byBhcHBseSBjaGFuZ2VzIG9yIFwiblwiIHRvIGNhbmNlbCcsXG4gICAgfSxcbiAgICBkZWZhdWx0UmVzcG9uc2U6IGZhbHNlLFxuICB9KTtcbiAgaWYgKHVzZXJBY2NlcHRlZCkge1xuICAgIGF3YWl0IG1vZGlmeVZhbHVlcyhwYXJhbXMsIGZsYWdOYW1lcyk7XG4gICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuaW5mbygnRmxhZyB2YWx1ZShzKSB1cGRhdGVkIHN1Y2Nlc3NmdWxseS4nKTtcbiAgfSBlbHNlIHtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKCdPcGVyYXRpb24gY2FuY2VsbGVkJyk7XG4gIH1cblxuICBjb25zdCBvcmlnaW5hbERpciA9IHBhdGguam9pbihwcm9jZXNzLmN3ZCgpLCAnb3JpZ2luYWwnKTtcbiAgY29uc3QgdGVtcERpciA9IHBhdGguam9pbihwcm9jZXNzLmN3ZCgpLCAndGVtcCcpO1xuXG4gIGF3YWl0IGZzLnJlbW92ZShvcmlnaW5hbERpcik7XG4gIGF3YWl0IGZzLnJlbW92ZSh0ZW1wRGlyKTtcbn1cblxuYXN5bmMgZnVuY3Rpb24gbW9kaWZ5VmFsdWVzKHBhcmFtczogRmxhZ09wZXJhdGlvbnNQYXJhbXMsIGZsYWdOYW1lczogc3RyaW5nW10pOiBQcm9taXNlPHZvaWQ+IHtcbiAgY29uc3QgeyBmbGFnRGF0YSwgaW9IZWxwZXIsIHZhbHVlLCByZWNvbW1lbmRlZCB9ID0gcGFyYW1zO1xuICBjb25zdCBjZGtKc29uUGF0aCA9IHBhdGguam9pbihwcm9jZXNzLmN3ZCgpLCAnY2RrLmpzb24nKTtcbiAgY29uc3QgY2RrSnNvbkNvbnRlbnQgPSBhd2FpdCBmcy5yZWFkRmlsZShjZGtKc29uUGF0aCwgJ3V0Zi04Jyk7XG4gIGNvbnN0IGNka0pzb24gPSBKU09OLnBhcnNlKGNka0pzb25Db250ZW50KTtcblxuICBpZiAoZmxhZ05hbWVzLmxlbmd0aCA9PSAxKSB7XG4gICAgY29uc3QgYm9vbFZhbHVlID0gdG9Cb29sZWFuVmFsdWUodmFsdWUpO1xuICAgIGNka0pzb24uY29udGV4dFtTdHJpbmcoZmxhZ05hbWVzWzBdKV0gPSBib29sVmFsdWU7XG5cbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKGBTZXR0aW5nIGZsYWcgJyR7ZmxhZ05hbWVzfScgdG86ICR7Ym9vbFZhbHVlfWApO1xuICB9IGVsc2Uge1xuICAgIGZvciAoY29uc3QgZmxhZ05hbWUgb2YgZmxhZ05hbWVzKSB7XG4gICAgICBjb25zdCBmbGFnID0gZmxhZ0RhdGEuZmluZChmID0+IGYubmFtZSA9PT0gZmxhZ05hbWUpO1xuICAgICAgY29uc3QgbmV3VmFsdWUgPSByZWNvbW1lbmRlZFxuICAgICAgICA/IHRvQm9vbGVhblZhbHVlKGZsYWchLnJlY29tbWVuZGVkVmFsdWUpXG4gICAgICAgIDogU3RyaW5nKGZsYWchLnVuY29uZmlndXJlZEJlaGF2ZXNMaWtlPy52MikgPT09ICd0cnVlJztcbiAgICAgIGNka0pzb24uY29udGV4dFtmbGFnTmFtZV0gPSBuZXdWYWx1ZTtcbiAgICB9XG4gIH1cblxuICBhd2FpdCBmcy53cml0ZUZpbGUoY2RrSnNvblBhdGgsIEpTT04uc3RyaW5naWZ5KGNka0pzb24sIG51bGwsIDIpLCAndXRmLTgnKTtcbn1cblxuZnVuY3Rpb24gZ2V0RmxhZ1NvcnRPcmRlcihmbGFnOiBGZWF0dXJlRmxhZyk6IG51bWJlciB7XG4gIGlmIChmbGFnLnVzZXJWYWx1ZSA9PT0gdW5kZWZpbmVkKSB7XG4gICAgcmV0dXJuIDM7XG4gIH0gZWxzZSBpZiAoaXNVc2VyVmFsdWVFcXVhbFRvUmVjb21tZW5kZWQoZmxhZykpIHtcbiAgICByZXR1cm4gMTtcbiAgfSBlbHNlIHtcbiAgICByZXR1cm4gMjtcbiAgfVxufVxuXG5hc3luYyBmdW5jdGlvbiBkaXNwbGF5RmxhZ1RhYmxlKGZsYWdzOiBGZWF0dXJlRmxhZ1tdLCBpb0hlbHBlcjogSW9IZWxwZXIpOiBQcm9taXNlPHZvaWQ+IHtcbiAgY29uc3QgZmlsdGVyZWRGbGFncyA9IGZsYWdzLmZpbHRlcihmbGFnID0+IGZsYWcudW5jb25maWd1cmVkQmVoYXZlc0xpa2U/LnYyICE9PSBmbGFnLnJlY29tbWVuZGVkVmFsdWUpO1xuXG4gIGNvbnN0IHNvcnRlZEZsYWdzID0gWy4uLmZpbHRlcmVkRmxhZ3NdLnNvcnQoKGEsIGIpID0+IHtcbiAgICBjb25zdCBvcmRlckEgPSBnZXRGbGFnU29ydE9yZGVyKGEpO1xuICAgIGNvbnN0IG9yZGVyQiA9IGdldEZsYWdTb3J0T3JkZXIoYik7XG5cbiAgICBpZiAob3JkZXJBICE9PSBvcmRlckIpIHtcbiAgICAgIHJldHVybiBvcmRlckEgLSBvcmRlckI7XG4gICAgfVxuICAgIGlmIChhLm1vZHVsZSAhPT0gYi5tb2R1bGUpIHtcbiAgICAgIHJldHVybiBhLm1vZHVsZS5sb2NhbGVDb21wYXJlKGIubW9kdWxlKTtcbiAgICB9XG4gICAgcmV0dXJuIGEubmFtZS5sb2NhbGVDb21wYXJlKGIubmFtZSk7XG4gIH0pO1xuXG4gIGNvbnN0IHJvd3M6IHN0cmluZ1tdW10gPSBbXTtcbiAgcm93cy5wdXNoKFsnRmVhdHVyZSBGbGFnIE5hbWUnLCAnUmVjb21tZW5kZWQgVmFsdWUnLCAnVXNlciBWYWx1ZSddKTtcbiAgbGV0IGN1cnJlbnRNb2R1bGUgPSAnJztcblxuICBzb3J0ZWRGbGFncy5mb3JFYWNoKChmbGFnKSA9PiB7XG4gICAgaWYgKGZsYWcubW9kdWxlICE9PSBjdXJyZW50TW9kdWxlKSB7XG4gICAgICByb3dzLnB1c2goW2NoYWxrLmJvbGQoYE1vZHVsZTogJHtmbGFnLm1vZHVsZX1gKSwgJycsICcnXSk7XG4gICAgICBjdXJyZW50TW9kdWxlID0gZmxhZy5tb2R1bGU7XG4gICAgfVxuICAgIHJvd3MucHVzaChbXG4gICAgICBgICAke2ZsYWcubmFtZX1gLFxuICAgICAgU3RyaW5nKGZsYWcucmVjb21tZW5kZWRWYWx1ZSksXG4gICAgICBmbGFnLnVzZXJWYWx1ZSA9PT0gdW5kZWZpbmVkID8gJzx1bnNldD4nIDogU3RyaW5nKGZsYWcudXNlclZhbHVlKSxcbiAgICBdKTtcbiAgfSk7XG5cbiAgY29uc3QgZm9ybWF0dGVkVGFibGUgPSBmb3JtYXRUYWJsZShyb3dzLCB1bmRlZmluZWQsIHRydWUpO1xuICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKGZvcm1hdHRlZFRhYmxlKTtcbn1cblxuZXhwb3J0IGFzeW5jIGZ1bmN0aW9uIGRpc3BsYXlGbGFncyhwYXJhbXM6IEZsYWdPcGVyYXRpb25zUGFyYW1zKTogUHJvbWlzZTx2b2lkPiB7XG4gIGNvbnN0IHsgZmxhZ0RhdGEsIGlvSGVscGVyLCBmbGFnTmFtZSwgYWxsIH0gPSBwYXJhbXM7XG5cbiAgaWYgKGZsYWdOYW1lICYmIGZsYWdOYW1lLmxlbmd0aCA+IDApIHtcbiAgICBjb25zdCBtYXRjaGluZ0ZsYWdzID0gZmxhZ0RhdGEuZmlsdGVyKGYgPT5cbiAgICAgIGZsYWdOYW1lLnNvbWUoc2VhcmNoVGVybSA9PiBmLm5hbWUudG9Mb3dlckNhc2UoKS5pbmNsdWRlcyhzZWFyY2hUZXJtLnRvTG93ZXJDYXNlKCkpKSxcbiAgICApO1xuXG4gICAgaWYgKG1hdGNoaW5nRmxhZ3MubGVuZ3RoID09PSAwKSB7XG4gICAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5lcnJvcihgRmxhZyBtYXRjaGluZyBcIiR7ZmxhZ05hbWUuam9pbignLCAnKX1cIiBub3QgZm91bmQuYCk7XG4gICAgICByZXR1cm47XG4gICAgfVxuXG4gICAgaWYgKG1hdGNoaW5nRmxhZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICBjb25zdCBmbGFnID0gbWF0Y2hpbmdGbGFnc1swXTtcbiAgICAgIGF3YWl0IGlvSGVscGVyLmRlZmF1bHRzLmluZm8oYEZsYWcgbmFtZTogJHtmbGFnLm5hbWV9YCk7XG4gICAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKGBEZXNjcmlwdGlvbjogJHtmbGFnLmV4cGxhbmF0aW9ufWApO1xuICAgICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuaW5mbyhgUmVjb21tZW5kZWQgdmFsdWU6ICR7ZmxhZy5yZWNvbW1lbmRlZFZhbHVlfWApO1xuICAgICAgYXdhaXQgaW9IZWxwZXIuZGVmYXVsdHMuaW5mbyhgVXNlciB2YWx1ZTogJHtmbGFnLnVzZXJWYWx1ZX1gKTtcbiAgICAgIHJldHVybjtcbiAgICB9XG5cbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKGBGb3VuZCAke21hdGNoaW5nRmxhZ3MubGVuZ3RofSBmbGFncyBtYXRjaGluZyBcIiR7ZmxhZ05hbWUuam9pbignLCAnKX1cIjpgKTtcbiAgICBhd2FpdCBkaXNwbGF5RmxhZ1RhYmxlKG1hdGNoaW5nRmxhZ3MsIGlvSGVscGVyKTtcbiAgICByZXR1cm47XG4gIH1cblxuICBsZXQgZmxhZ3NUb0Rpc3BsYXk6IEZlYXR1cmVGbGFnW107XG4gIGlmIChhbGwpIHtcbiAgICBmbGFnc1RvRGlzcGxheSA9IGZsYWdEYXRhO1xuICB9IGVsc2Uge1xuICAgIGZsYWdzVG9EaXNwbGF5ID0gZmxhZ0RhdGEuZmlsdGVyKGZsYWcgPT5cbiAgICAgIGZsYWcudXNlclZhbHVlID09PSB1bmRlZmluZWQgfHwgIWlzVXNlclZhbHVlRXF1YWxUb1JlY29tbWVuZGVkKGZsYWcpLFxuICAgICk7XG4gIH1cblxuICBhd2FpdCBkaXNwbGF5RmxhZ1RhYmxlKGZsYWdzVG9EaXNwbGF5LCBpb0hlbHBlcik7XG5cbiAgLy8gQWRkIGhlbHBmdWwgbWVzc2FnZSBhZnRlciBlbXB0eSB0YWJsZSB3aGVuIG5vdCB1c2luZyAtLWFsbFxuICBpZiAoIWFsbCAmJiBmbGFnc1RvRGlzcGxheS5sZW5ndGggPT09IDApIHtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKCcnKTtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKCfinIUgQWxsIGZlYXR1cmUgZmxhZ3MgYXJlIGFscmVhZHkgc2V0IHRvIHRoZWlyIHJlY29tbWVuZGVkIHZhbHVlcy4nKTtcbiAgICBhd2FpdCBpb0hlbHBlci5kZWZhdWx0cy5pbmZvKCdVc2UgXFwnY2RrIGZsYWdzIC0tYWxsIC0tdW5zdGFibGU9ZmxhZ3NcXCcgdG8gc2VlIGFsbCBmbGFncyBhbmQgdGhlaXIgY3VycmVudCB2YWx1ZXMuJyk7XG4gIH1cbn1cblxuZnVuY3Rpb24gaXNVc2VyVmFsdWVFcXVhbFRvUmVjb21tZW5kZWQoZmxhZzogRmVhdHVyZUZsYWcpOiBib29sZWFuIHtcbiAgcmV0dXJuIFN0cmluZyhmbGFnLnVzZXJWYWx1ZSkgPT09IFN0cmluZyhmbGFnLnJlY29tbWVuZGVkVmFsdWUpO1xufVxuXG5mdW5jdGlvbiB0b0Jvb2xlYW5WYWx1ZSh2YWx1ZTogdW5rbm93bik6IGJvb2xlYW4ge1xuICBpZiAodHlwZW9mIHZhbHVlID09PSAnYm9vbGVhbicpIHtcbiAgICByZXR1cm4gdmFsdWU7XG4gIH1cbiAgaWYgKHR5cGVvZiB2YWx1ZSA9PT0gJ3N0cmluZycpIHtcbiAgICByZXR1cm4gdmFsdWUudG9Mb3dlckNhc2UoKSA9PT0gJ3RydWUnO1xuICB9XG4gIHJldHVybiBmYWxzZTtcbn1cblxuZnVuY3Rpb24gaXNCb29sZWFuRmxhZyhmbGFnOiBGZWF0dXJlRmxhZyk6IGJvb2xlYW4ge1xuICBjb25zdCByZWNvbW1lbmRlZCA9IGZsYWcucmVjb21tZW5kZWRWYWx1ZTtcbiAgcmV0dXJuIHR5cGVvZiByZWNvbW1lbmRlZCA9PT0gJ2Jvb2xlYW4nIHx8XG4gICAgcmVjb21tZW5kZWQgPT09ICd0cnVlJyB8fFxuICAgIHJlY29tbWVuZGVkID09PSAnZmFsc2UnO1xufVxuIl19