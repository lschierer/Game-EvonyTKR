"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.CloudAssembly = exports.ExtendedStackSelection = exports.DefaultSelection = void 0;
const toolkit_lib_1 = require("@aws-cdk/toolkit-lib");
const minimatch_1 = require("minimatch");
const semver = require("semver");
const cloud_assembly_1 = require("../api/cloud-assembly");
const util_1 = require("../util");
var DefaultSelection;
(function (DefaultSelection) {
    /**
     * Returns an empty selection in case there are no selectors.
     */
    DefaultSelection["None"] = "none";
    /**
     * If the app includes a single stack, returns it. Otherwise throws an exception.
     * This behavior is used by "deploy".
     */
    DefaultSelection["OnlySingle"] = "single";
    /**
     * Returns all stacks in the main (top level) assembly only.
     */
    DefaultSelection["MainAssembly"] = "main";
    /**
     * If no selectors are provided, returns all stacks in the app,
     * including stacks inside nested assemblies.
     */
    DefaultSelection["AllStacks"] = "all";
})(DefaultSelection || (exports.DefaultSelection = DefaultSelection = {}));
/**
 * When selecting stacks, what other stacks to include because of dependencies
 */
var ExtendedStackSelection;
(function (ExtendedStackSelection) {
    /**
     * Don't select any extra stacks
     */
    ExtendedStackSelection[ExtendedStackSelection["None"] = 0] = "None";
    /**
     * Include stacks that this stack depends on
     */
    ExtendedStackSelection[ExtendedStackSelection["Upstream"] = 1] = "Upstream";
    /**
     * Include stacks that depend on this stack
     */
    ExtendedStackSelection[ExtendedStackSelection["Downstream"] = 2] = "Downstream";
})(ExtendedStackSelection || (exports.ExtendedStackSelection = ExtendedStackSelection = {}));
/**
 * A single Cloud Assembly and the operations we do on it to deploy the artifacts inside
 */
class CloudAssembly extends cloud_assembly_1.BaseStackAssembly {
    async selectStacks(selector, options) {
        const asm = this.assembly;
        const topLevelStacks = asm.stacks;
        const stacks = semver.major(asm.version) < 10 ? asm.stacks : asm.stacksRecursively;
        const allTopLevel = selector.allTopLevel ?? false;
        const patterns = CloudAssembly.sanitizePatterns(selector.patterns);
        if (stacks.length === 0) {
            if (options.ignoreNoStacks) {
                return new cloud_assembly_1.StackCollection(this, []);
            }
            throw new toolkit_lib_1.ToolkitError('This app contains no stacks');
        }
        if (allTopLevel) {
            return this.selectTopLevelStacks(stacks, topLevelStacks, options.extend);
        }
        else if (patterns.length > 0) {
            return this.selectMatchingStacks(stacks, patterns, options.extend);
        }
        else {
            return this.selectDefaultStacks(stacks, topLevelStacks, options.defaultBehavior);
        }
    }
    async selectTopLevelStacks(stacks, topLevelStacks, extend = ExtendedStackSelection.None) {
        if (topLevelStacks.length > 0) {
            return this.extendStacks(topLevelStacks, stacks, extend);
        }
        else {
            throw new toolkit_lib_1.ToolkitError('No stack found in the main cloud assembly. Use "list" to print manifest');
        }
    }
    async selectMatchingStacks(stacks, patterns, extend = ExtendedStackSelection.None) {
        const matchingPattern = (pattern) => (stack) => (0, minimatch_1.minimatch)(stack.hierarchicalId, pattern);
        const matchedStacks = (0, util_1.flatten)(patterns.map(pattern => stacks.filter(matchingPattern(pattern))));
        return this.extendStacks(matchedStacks, stacks, extend);
    }
    selectDefaultStacks(stacks, topLevelStacks, defaultSelection) {
        switch (defaultSelection) {
            case DefaultSelection.MainAssembly:
                return new cloud_assembly_1.StackCollection(this, topLevelStacks);
            case DefaultSelection.AllStacks:
                return new cloud_assembly_1.StackCollection(this, stacks);
            case DefaultSelection.None:
                return new cloud_assembly_1.StackCollection(this, []);
            case DefaultSelection.OnlySingle:
                if (topLevelStacks.length === 1) {
                    return new cloud_assembly_1.StackCollection(this, topLevelStacks);
                }
                else {
                    throw new toolkit_lib_1.ToolkitError('Since this app includes more than a single stack, specify which stacks to use (wildcards are supported) or specify `--all`\n' +
                        `Stacks: ${stacks.map(x => x.hierarchicalId).join(' · ')}`);
                }
            default:
                throw new toolkit_lib_1.ToolkitError(`invalid default behavior: ${defaultSelection}`);
        }
    }
}
exports.CloudAssembly = CloudAssembly;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2xvdWQtYXNzZW1ibHkuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyJjbG91ZC1hc3NlbWJseS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7QUFDQSxzREFBb0Q7QUFDcEQseUNBQXNDO0FBQ3RDLGlDQUFpQztBQUNqQywwREFBMkU7QUFDM0Usa0NBQWtDO0FBRWxDLElBQVksZ0JBc0JYO0FBdEJELFdBQVksZ0JBQWdCO0lBQzFCOztPQUVHO0lBQ0gsaUNBQWEsQ0FBQTtJQUViOzs7T0FHRztJQUNILHlDQUFxQixDQUFBO0lBRXJCOztPQUVHO0lBQ0gseUNBQXFCLENBQUE7SUFFckI7OztPQUdHO0lBQ0gscUNBQWlCLENBQUE7QUFDbkIsQ0FBQyxFQXRCVyxnQkFBZ0IsZ0NBQWhCLGdCQUFnQixRQXNCM0I7QUFzQkQ7O0dBRUc7QUFDSCxJQUFZLHNCQWVYO0FBZkQsV0FBWSxzQkFBc0I7SUFDaEM7O09BRUc7SUFDSCxtRUFBSSxDQUFBO0lBRUo7O09BRUc7SUFDSCwyRUFBUSxDQUFBO0lBRVI7O09BRUc7SUFDSCwrRUFBVSxDQUFBO0FBQ1osQ0FBQyxFQWZXLHNCQUFzQixzQ0FBdEIsc0JBQXNCLFFBZWpDO0FBa0JEOztHQUVHO0FBQ0gsTUFBYSxhQUFjLFNBQVEsa0NBQWlCO0lBQzNDLEtBQUssQ0FBQyxZQUFZLENBQUMsUUFBdUIsRUFBRSxPQUE0QjtRQUM3RSxNQUFNLEdBQUcsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDO1FBQzFCLE1BQU0sY0FBYyxHQUFHLEdBQUcsQ0FBQyxNQUFNLENBQUM7UUFDbEMsTUFBTSxNQUFNLEdBQUcsTUFBTSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUMsaUJBQWlCLENBQUM7UUFDbkYsTUFBTSxXQUFXLEdBQUcsUUFBUSxDQUFDLFdBQVcsSUFBSSxLQUFLLENBQUM7UUFD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Qyw4SEFBOEg7d0JBQ3JKLFdBQVcsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxjQUFjLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDLEVBQUUsQ0FBQyxDQUFDO2dCQUM5RCxDQUFDO1lBQ0g7Z0JBQ0UsTUFBTSxJQUFJLDBCQUFZLENBQUMsNkJBQTZCLGdCQUFnQixFQUFFLENBQUMsQ0FBQztRQUM1RSxDQUFDO0lBQ0gsQ0FBQztDQUNGO0FBckVELHNDQXFFQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB0eXBlICogYXMgY3hhcGkgZnJvbSAnQGF3cy1jZGsvY3gtYXBpJztcbmltcG9ydCB7IFRvb2xraXRFcnJvciB9IGZyb20gJ0Bhd3MtY2RrL3Rvb2xraXQtbGliJztcbmltcG9ydCB7IG1pbmltYXRjaCB9IGZyb20gJ21pbmltYXRjaCc7XG5pbXBvcnQgKiBhcyBzZW12ZXIgZnJvbSAnc2VtdmVyJztcbmltcG9ydCB7IEJhc2VTdGFja0Fzc2VtYmx5LCBTdGFja0NvbGxlY3Rpb24gfSBmcm9tICcuLi9hcGkvY2xvdWQtYXNzZW1ibHknO1xuaW1wb3J0IHsgZmxhdHRlbiB9IGZyb20gJy4uL3V0aWwnO1xuXG5leHBvcnQgZW51bSBEZWZhdWx0U2VsZWN0aW9uIHtcbiAgLyoqXG4gICAqIFJldHVybnMgYW4gZW1wdHkgc2VsZWN0aW9uIGluIGNhc2UgdGhlcmUgYXJlIG5vIHNlbGVjdG9ycy5cbiAgICovXG4gIE5vbmUgPSAnbm9uZScsXG5cbiAgLyoqXG4gICAqIElmIHRoZSBhcHAgaW5jbHVkZXMgYSBzaW5nbGUgc3RhY2ssIHJldHVybnMgaXQuIE90aGVyd2lzZSB0aHJvd3MgYW4gZXhjZXB0aW9uLlxuICAgKiBUaGlzIGJlaGF2aW9yIGlzIHVzZWQgYnkgXCJkZXBsb3lcIi5cbiAgICovXG4gIE9ubHlTaW5nbGUgPSAnc2luZ2xlJyxcblxuICAvKipcbiAgICogUmV0dXJucyBhbGwgc3RhY2tzIGluIHRoZSBtYWluICh0b3AgbGV2ZWwpIGFzc2VtYmx5IG9ubHkuXG4gICAqL1xuICBNYWluQXNzZW1ibHkgPSAnbWFpbicsXG5cbiAgLyoqXG4gICAqIElmIG5vIHNlbGVjdG9ycyBhcmUgcHJvdmlkZWQsIHJldHVybnMgYWxsIHN0YWNrcyBpbiB0aGUgYXBwLFxuICAgKiBpbmNsdWRpbmcgc3RhY2tzIGluc2lkZSBuZXN0ZWQgYXNzZW1ibGllcy5cbiAgICovXG4gIEFsbFN0YWNrcyA9ICdhbGwnLFxufVxuXG5leHBvcnQgaW50ZXJmYWNlIFNlbGVjdFN0YWNrc09wdGlvbnMge1xuICAvKipcbiAgICogRXh0ZW5kIHRoZSBzZWxlY3Rpb24gdG8gdXBzdHJlYWQvZG93bnN0cmVhbSBzdGFja3NcbiAgICogQGRlZmF1bHQgRXh0ZW5kZWRTdGFja1NlbGVjdGlvbi5Ob25lIG9ubHkgc2VsZWN0IHRoZSBzcGVjaWZpZWQgc3RhY2tzLlxuICAgKi9cbiAgZXh0ZW5kPzogRXh0ZW5kZWRTdGFja1NlbGVjdGlvbjtcblxuICAvKipcbiAgICogVGhlIGJlaGF2aW9yIGlmIG5vIHNlbGVjdG9ycyBhcmUgcHJvdmlkZWQuXG4gICAqL1xuICBkZWZhdWx0QmVoYXZpb3I6IERlZmF1bHRTZWxlY3Rpb247XG5cbiAgLyoqXG4gICAqIFdoZXRoZXIgdG8gZGVwbG95IGlmIHRoZSBhcHAgY29udGFpbnMgbm8gc3RhY2tzLlxuICAgKlxuICAgKiBAZGVmYXVsdCBmYWxzZVxuICAgKi9cbiAgaWdub3JlTm9TdGFja3M/OiBib29sZWFuO1xufVxuXG4vKipcbiAqIFdoZW4gc2VsZWN0aW5nIHN0YWNrcywgd2hhdCBvdGhlciBzdGFja3MgdG8gaW5jbHVkZSBiZWNhdXNlIG9mIGRlcGVuZGVuY2llc1xuICovXG5leHBvcnQgZW51bSBFeHRlbmRlZFN0YWNrU2VsZWN0aW9uIHtcbiAgLyoqXG4gICAqIERvbid0IHNlbGVjdCBhbnkgZXh0cmEgc3RhY2tzXG4gICAqL1xuICBOb25lLFxuXG4gIC8qKlxuICAgKiBJbmNsdWRlIHN0YWNrcyB0aGF0IHRoaXMgc3RhY2sgZGVwZW5kcyBvblxuICAgKi9cbiAgVXBzdHJlYW0sXG5cbiAgLyoqXG4gICAqIEluY2x1ZGUgc3RhY2tzIHRoYXQgZGVwZW5kIG9uIHRoaXMgc3RhY2tcbiAgICovXG4gIERvd25zdHJlYW0sXG59XG5cbi8qKlxuICogQSBzcGVjaWZpY2F0aW9uIG9mIHdoaWNoIHN0YWNrcyBzaG91bGQgYmUgc2VsZWN0ZWRcbiAqL1xuZXhwb3J0IGludGVyZmFjZSBTdGFja1NlbGVjdG9yIHtcbiAgLyoqXG4gICAqIFdoZXRoZXIgYWxsIHN0YWNrcyBhdCB0aGUgdG9wIGxldmVsIGFzc2VtYmx5IHNob3VsZFxuICAgKiBiZSBzZWxlY3RlZCBhbmQgbm90aGluZyBlbHNlXG4gICAqL1xuICBhbGxUb3BMZXZlbD86IGJvb2xlYW47XG5cbiAgLyoqXG4gICAqIEEgbGlzdCBvZiBwYXR0ZXJucyB0byBtYXRjaCB0aGUgc3RhY2sgaGllcmFyY2hpY2FsIGlkc1xuICAgKi9cbiAgcGF0dGVybnM6IHN0cmluZ1tdO1xufVxuXG4vKipcbiAqIEEgc2luZ2xlIENsb3VkIEFzc2VtYmx5IGFuZCB0aGUgb3BlcmF0aW9ucyB3ZSBkbyBvbiBpdCB0byBkZXBsb3kgdGhlIGFydGlmYWN0cyBpbnNpZGVcbiAqL1xuZXhwb3J0IGNsYXNzIENsb3VkQXNzZW1ibHkgZXh0ZW5kcyBCYXNlU3RhY2tBc3NlbWJseSB7XG4gIHB1YmxpYyBhc3luYyBzZWxlY3RTdGFja3Moc2VsZWN0b3I6IFN0YWNrU2VsZWN0b3IsIG9wdGlvbnM6IFNlbGVjdFN0YWNrc09wdGlvbnMpOiBQcm9taXNlPFN0YWNrQ29sbGVjdGlvbj4ge1xuICAgIGNvbnN0IGFzbSA9IHRoaXMuYXNzZW1ibHk7XG4gICAgY29uc3QgdG9wTGV2ZWxTdGFja3MgPSBhc20uc3RhY2tzO1xuICAgIGNvbnN0IHN0YWNrcyA9IHNlbXZlci5tYWpvcihhc20udmVyc2lvbikgPCAxMCA/IGFzbS5zdGFja3MgOiBhc20uc3RhY2tzUmVjdXJzaXZlbHk7XG4gICAgY29uc3QgYWxsVG9wTGV2ZWwgPSBzZWxlY3Rvci5hbGxUb3BMZXZlbCA/PyBmYWxzZTtcbiAgICBjb25zdCBwYXR0ZXJucyA9IENsb3VkQXNzZW1ibHkuc2FuaXRpemVQYXR0ZXJucyhzZWxlY3Rvci5wYXR0ZXJucyk7XG5cbiAgICBpZiAoc3RhY2tzLmxlbmd0aCA9PT0gMCkge1xuICAgICAgaWYgKG9wdGlvbnMuaWdub3JlTm9TdGFja3MpIHtcbiAgICAgICAgcmV0dXJuIG5ldyBTdGFja0NvbGxlY3Rpb24odGhpcywgW10pO1xuICAgICAgfVxuICAgICAgdGhyb3cgbmV3IFRvb2xraXRFcnJvcignVGhpcyBhcHAgY29udGFpbnMgbm8gc3RhY2tzJyk7XG4gICAgfVxuXG4gICAgaWYgKGFsbFRvcExldmVsKSB7XG4gICAgICByZXR1cm4gdGhpcy5zZWxlY3RUb3BMZXZlbFN0YWNrcyhzdGFja3MsIHRvcExldmVsU3RhY2tzLCBvcHRpb25zLmV4dGVuZCk7XG4gICAgfSBlbHNlIGlmIChwYXR0ZXJucy5sZW5ndGggPiAwKSB7XG4gICAgICByZXR1cm4gdGhpcy5zZWxlY3RNYXRjaGluZ1N0YWNrcyhzdGFja3MsIHBhdHRlcm5zLCBvcHRpb25zLmV4dGVuZCk7XG4gICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiB0aGlzLnNlbGVjdERlZmF1bHRTdGFja3Moc3RhY2tzLCB0b3BMZXZlbFN0YWNrcywgb3B0aW9ucy5kZWZhdWx0QmVoYXZpb3IpO1xuICAgIH1cbiAgfVxuXG4gIHByaXZhdGUgYXN5bmMgc2VsZWN0VG9wTGV2ZWxTdGFja3MoXG4gICAgc3RhY2tzOiBjeGFwaS5DbG91ZEZvcm1hdGlvblN0YWNrQXJ0aWZhY3RbXSxcbiAgICB0b3BMZXZlbFN0YWNrczogY3hhcGkuQ2xvdWRGb3JtYXRpb25TdGFja0FydGlmYWN0W10sXG4gICAgZXh0ZW5kOiBFeHRlbmRlZFN0YWNrU2VsZWN0aW9uID0gRXh0ZW5kZWRTdGFja1NlbGVjdGlvbi5Ob25lLFxuICApOiBQcm9taXNlPFN0YWNrQ29sbGVjdGlvbj4ge1xuICAgIGlmICh0b3BMZXZlbFN0YWNrcy5sZW5ndGggPiAwKSB7XG4gICAgICByZXR1cm4gdGhpcy5leHRlbmRTdGFja3ModG9wTGV2ZWxTdGFja3MsIHN0YWNrcywgZXh0ZW5kKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhyb3cgbmV3IFRvb2xraXRFcnJvcignTm8gc3RhY2sgZm91bmQgaW4gdGhlIG1haW4gY2xvdWQgYXNzZW1ibHkuIFVzZSBcImxpc3RcIiB0byBwcmludCBtYW5pZmVzdCcpO1xuICAgIH1cbiAgfVxuXG4gIHByb3RlY3RlZCBhc3luYyBzZWxlY3RNYXRjaGluZ1N0YWNrcyhcbiAgICBzdGFja3M6IGN4YXBpLkNsb3VkRm9ybWF0aW9uU3RhY2tBcnRpZmFjdFtdLFxuICAgIHBhdHRlcm5zOiBzdHJpbmdbXSxcbiAgICBleHRlbmQ6IEV4dGVuZGVkU3RhY2tTZWxlY3Rpb24gPSBFeHRlbmRlZFN0YWNrU2VsZWN0aW9uLk5vbmUsXG4gICk6IFByb21pc2U8U3RhY2tDb2xsZWN0aW9uPiB7XG4gICAgY29uc3QgbWF0Y2hpbmdQYXR0ZXJuID0gKHBhdHRlcm46IHN0cmluZykgPT4gKHN0YWNrOiBjeGFwaS5DbG91ZEZvcm1hdGlvblN0YWNrQXJ0aWZhY3QpID0+IG1pbmltYXRjaChzdGFjay5oaWVyYXJjaGljYWxJZCwgcGF0dGVybik7XG4gICAgY29uc3QgbWF0Y2hlZFN0YWNrcyA9IGZsYXR0ZW4ocGF0dGVybnMubWFwKHBhdHRlcm4gPT4gc3RhY2tzLmZpbHRlcihtYXRjaGluZ1BhdHRlcm4ocGF0dGVybikpKSk7XG4gICAgcmV0dXJuIHRoaXMuZXh0ZW5kU3RhY2tzKG1hdGNoZWRTdGFja3MsIHN0YWNrcywgZXh0ZW5kKTtcbiAgfVxuXG4gIHByaXZhdGUgc2VsZWN0RGVmYXVsdFN0YWNrcyhcbiAgICBzdGFja3M6IGN4YXBpLkNsb3VkRm9ybWF0aW9uU3RhY2tBcnRpZmFjdFtdLFxuICAgIHRvcExldmVsU3RhY2tzOiBjeGFwaS5DbG91ZEZvcm1hdGlvblN0YWNrQXJ0aWZhY3RbXSxcbiAgICBkZWZhdWx0U2VsZWN0aW9uOiBEZWZhdWx0U2VsZWN0aW9uLFxuICApIHtcbiAgICBzd2l0Y2ggKGRlZmF1bHRTZWxlY3Rpb24pIHtcbiAgICAgIGNhc2UgRGVmYXVsdFNlbGVjdGlvbi5NYWluQXNzZW1ibHk6XG4gICAgICAgIHJldHVybiBuZXcgU3RhY2tDb2xsZWN0aW9uKHRoaXMsIHRvcExldmVsU3RhY2tzKTtcbiAgICAgIGNhc2UgRGVmYXVsdFNlbGVjdGlvbi5BbGxTdGFja3M6XG4gICAgICAgIHJldHVybiBuZXcgU3RhY2tDb2xsZWN0aW9uKHRoaXMsIHN0YWNrcyk7XG4gICAgICBjYXNlIERlZmF1bHRTZWxlY3Rpb24uTm9uZTpcbiAgICAgICAgcmV0dXJuIG5ldyBTdGFja0NvbGxlY3Rpb24odGhpcywgW10pO1xuICAgICAgY2FzZSBEZWZhdWx0U2VsZWN0aW9uLk9ubHlTaW5nbGU6XG4gICAgICAgIGlmICh0b3BMZXZlbFN0YWNrcy5sZW5ndGggPT09IDEpIHtcbiAgICAgICAgICByZXR1cm4gbmV3IFN0YWNrQ29sbGVjdGlvbih0aGlzLCB0b3BMZXZlbFN0YWNrcyk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgdGhyb3cgbmV3IFRvb2xraXRFcnJvcignU2luY2UgdGhpcyBhcHAgaW5jbHVkZXMgbW9yZSB0aGFuIGEgc2luZ2xlIHN0YWNrLCBzcGVjaWZ5IHdoaWNoIHN0YWNrcyB0byB1c2UgKHdpbGRjYXJkcyBhcmUgc3VwcG9ydGVkKSBvciBzcGVjaWZ5IGAtLWFsbGBcXG4nICtcbiAgICAgICAgICBgU3RhY2tzOiAke3N0YWNrcy5tYXAoeCA9PiB4LmhpZXJhcmNoaWNhbElkKS5qb2luKCcgwrcgJyl9YCk7XG4gICAgICAgIH1cbiAgICAgIGRlZmF1bHQ6XG4gICAgICAgIHRocm93IG5ldyBUb29sa2l0RXJyb3IoYGludmFsaWQgZGVmYXVsdCBiZWhhdmlvcjogJHtkZWZhdWx0U2VsZWN0aW9ufWApO1xuICAgIH1cbiAgfVxufVxuIl19