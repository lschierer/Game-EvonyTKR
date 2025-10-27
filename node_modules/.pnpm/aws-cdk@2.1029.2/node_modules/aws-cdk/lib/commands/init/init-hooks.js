"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.invokeBuiltinHooks = invokeBuiltinHooks;
const path = require("path");
const toolkit_lib_1 = require("@aws-cdk/toolkit-lib");
const os_1 = require("./os");
const util_1 = require("../../util");
/**
 * Invoke hooks for the given init template
 *
 * Sometimes templates need more complex logic than just replacing tokens. A 'hook' can be
 * used to do additional processing other than copying files.
 *
 * Hooks used to be defined externally to the CLI, by running arbitrarily
 * substituted shell scripts in the target directory.
 *
 * In practice, they're all TypeScript files and all the same, and the dynamism
 * that the original solution allowed wasn't used at all. Worse, since the CLI
 * is now bundled the hooks can't even reuse code from the CLI libraries at all
 * anymore, so all shared code would have to be copy/pasted.
 *
 * Bundle hooks as built-ins into the CLI, so they get bundled and can take advantage
 * of all shared code.
 */
async function invokeBuiltinHooks(ioHelper, target, context) {
    switch (target.language) {
        case 'csharp':
            if (['app', 'sample-app'].includes(target.templateName)) {
                return dotnetAddProject(ioHelper, target.targetDirectory, context);
            }
            break;
        case 'fsharp':
            if (['app', 'sample-app'].includes(target.templateName)) {
                return dotnetAddProject(ioHelper, target.targetDirectory, context, 'fsproj');
            }
            break;
        case 'python':
            // We can't call this file 'requirements.template.txt' because Dependabot needs to be able to find it.
            // Therefore, keep the in-repo name but still substitute placeholders.
            await context.substitutePlaceholdersIn('requirements.txt');
            break;
        case 'java':
            // We can't call this file 'pom.template.xml'... for the same reason as Python above.
            await context.substitutePlaceholdersIn('pom.xml');
            break;
        case 'javascript':
        case 'typescript':
            // See above, but for 'package.json'.
            await context.substitutePlaceholdersIn('package.json');
    }
}
async function dotnetAddProject(ioHelper, targetDirectory, context, ext = 'csproj') {
    const pname = context.placeholder('name.PascalCased');
    const slnPath = path.join(targetDirectory, 'src', `${pname}.sln`);
    const csprojPath = path.join(targetDirectory, 'src', pname, `${pname}.${ext}`);
    // We retry this command a couple of times. It usually never fails, except on CI where
    // we sometimes see:
    //
    //   System.IO.IOException: The system cannot open the device or file specified. : 'NuGet-Migrations'
    //
    // This error can be caused by lack of permissions on a temporary directory,
    // but in our case it's intermittent so my guess is it is caused by multiple
    // invocations of the .NET CLI running in parallel, and trampling on each
    // other creating a Mutex. There is no fix, and it is annoyingly breaking our
    // CI regularly. Retry a couple of times to increase reliability.
    //
    // - https://github.com/dotnet/sdk/issues/43750
    // - https://github.com/dotnet/runtime/issues/80619
    // - https://github.com/dotnet/runtime/issues/91987
    const MAX_ATTEMPTS = 3;
    for (let attempt = 1;; attempt++) {
        try {
            await (0, os_1.shell)(ioHelper, ['dotnet', 'sln', slnPath, 'add', csprojPath]);
            return;
        }
        catch (e) {
            if (attempt === MAX_ATTEMPTS) {
                throw new toolkit_lib_1.ToolkitError(`Could not add project ${pname}.${ext} to solution ${pname}.sln. ${(0, util_1.formatErrorMessage)(e)}`);
            }
            // Sleep for a bit then try again
            await new Promise(resolve => setTimeout(resolve, 1000));
        }
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5pdC1ob29rcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbImluaXQtaG9va3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7QUFrREEsZ0RBOEJDO0FBaEZELDZCQUE2QjtBQUM3QixzREFBb0Q7QUFDcEQsNkJBQTZCO0FBRTdCLHFDQUFnRDtBQTZCaEQ7Ozs7Ozs7Ozs7Ozs7Ozs7R0FnQkc7QUFDSSxLQUFLLFVBQVUsa0JBQWtCLENBQUMsUUFBa0IsRUFBRSxNQUFrQixFQUFFLE9BQW9CO0lBQ25HLFFBQVEsTUFBTSxDQUFDLFFBQVEsRUFBRSxDQUFDO1FBQ3hCLEtBQUssUUFBUTtZQUNYLElBQUksQ0FBQyxLQUFLLEVBQUUsWUFBWSxDQUFDLENBQUMsUUFBUSxDQUFDLE1BQU0sQ0FBQyxZQUFZLENBQUMsRUFBRSxDQUFDO2dCQUN4RCxPQUFPLGdCQUFnQixDQUFDLFFBQVEsRUFBRSxNQUFNLENBQUMsZUFBZSxFQUFFLE9BQU8sQ0FBQyxDQUFDO1lBQ3JFLENBQUM7WUFDRCxNQUFNO1FBRVIsS0FBSyxRQUFRO1lBQ1gsSUFBSSxDQUFDLEtBQUssRUFBRSxZQUFZLENBQUMsQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFDLFlBQVksQ0FBQyxFQUFFLENBQUM7Z0JBQ3hELE9BQU8sZ0JBQWdCLENBQUMsUUFBUSxFQUFFLE1BQU0sQ0FBQyxlQUFlLEVBQUUsT0FBTyxFQUFFLFFBQVEsQ0FBQyxDQUFDO1lBQy9FLENBQUM7WUFDRCxNQUFNO1FBRVIsS0FBSyxRQUFRO1lBQ1gsc0dBQXNHO1lBQ3RHLHNFQUFzRTtZQUN0RSxNQUFNLE9BQU8sQ0FBQyx3QkFBd0IsQ0FBQyxrQkFBa0IsQ0FBQyxDQUFDO1lBQzNELE1BQU07UUFFUixLQUFLLE1BQU07WUFDVCxxRkFBcUY7WUFDckYsTUFBTSxPQUFPLENBQUMsd0JBQXdCLENBQUMsU0FBUyxDQUFDLENBQUM7WUFDbEQsTUFBTTtRQUVSLEtBQUssWUFBWSxDQUFDO1FBQ2xCLEtBQUssWUFBWTtZQUNmLHFDQUFxQztZQUNyQyxNQUFNLE9BQU8sQ0FBQyx3QkFBd0IsQ0FBQyxjQUFjLENBQUMsQ0FBQztJQUMzRCxDQUFDO0FBQ0gsQ0FBQztBQUVELEtBQUssVUFBVSxnQkFBZ0IsQ0FBQyxRQUFrQixFQUFFLGVBQXVCLEVBQUUsT0FBb0IsRUFBRSxHQUFHLEdBQUcsUUFBUTtJQUMvRyxNQUFNLEtBQUssR0FBRyxPQUFPLENBQUMsV0FBVyxDQUFDLGtCQUFrQixDQUFDLENBQUM7SUFDdEQsTUFBTSxPQUFPLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxlQUFlLEVBQUUsS0FBSyxFQUFFLEdBQUcsS0FBSyxNQUFNLENBQUMsQ0FBQztJQUNsRSxNQUFNLFVBQVUsR0FBRyxJQUFJLENBQUMsSUFBSSxDQUFDLGVBQWUsRUFBRSxLQUFLLEVBQUUsS0FBSyxFQUFFLEdBQUcsS0FBSyxJQUFJLEdBQUcsRUFBRSxDQUFDLENBQUM7SUFFL0Usc0ZBQXNGO0lBQ3RGLG9CQUFvQjtJQUNwQixFQUFFO0lBQ0YscUdBQXFHO0lBQ3JHLEVBQUU7SUFDRiw0RUFBNEU7SUFDNUUsNEVBQTRFO0lBQzVFLHlFQUF5RTtJQUN6RSw2RUFBNkU7SUFDN0UsaUVBQWlFO0lBQ2pFLEVBQUU7SUFDRiwrQ0FBK0M7SUFDL0MsbURBQW1EO0lBQ25ELG1EQUFtRDtJQUNuRCxNQUFNLFlBQVksR0FBRyxDQUFDLENBQUM7SUFDdkIsS0FBSyxJQUFJLE9BQU8sR0FBRyxDQUFDLEdBQUksT0FBTyxFQUFFLEVBQUUsQ0FBQztRQUNsQyxJQUFJLENBQUM7WUFDSCxNQUFNLElBQUEsVUFBSyxFQUFDLFFBQVEsRUFBRSxDQUFDLFFBQVEsRUFBRSxLQUFLLEVBQUUsT0FBTyxFQUFFLEtBQUssRUFBRSxVQUFVLENBQUMsQ0FBQyxDQUFDO1lBQ3JFLE9BQU87UUFDVCxDQUFDO1FBQUMsT0FBTyxDQUFNLEVBQUUsQ0FBQztZQUNoQixJQUFJLE9BQU8sS0FBSyxZQUFZLEVBQUUsQ0FBQztnQkFDN0IsTUFBTSxJQUFJLDBCQUFZLENBQUMseUJBQXlCLEtBQUssSUFBSSxHQUFHLGdCQUFnQixLQUFLLFNBQVMsSUFBQSx5QkFBa0IsRUFBQyxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUM7WUFDckgsQ0FBQztZQUVELGlDQUFpQztZQUNqQyxNQUFNLElBQUksT0FBTyxDQUFDLE9BQU8sQ0FBQyxFQUFFLENBQUMsVUFBVSxDQUFDLE9BQU8sRUFBRSxJQUFJLENBQUMsQ0FBQyxDQUFDO1FBQzFELENBQUM7SUFDSCxDQUFDO0FBQ0gsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCAqIGFzIHBhdGggZnJvbSAncGF0aCc7XG5pbXBvcnQgeyBUb29sa2l0RXJyb3IgfSBmcm9tICdAYXdzLWNkay90b29sa2l0LWxpYic7XG5pbXBvcnQgeyBzaGVsbCB9IGZyb20gJy4vb3MnO1xuaW1wb3J0IHR5cGUgeyBJb0hlbHBlciB9IGZyb20gJy4uLy4uL2FwaS1wcml2YXRlJztcbmltcG9ydCB7IGZvcm1hdEVycm9yTWVzc2FnZSB9IGZyb20gJy4uLy4uL3V0aWwnO1xuXG5leHBvcnQgdHlwZSBTdWJzdGl0dXRlUGxhY2Vob2xkZXJzID0gKC4uLmZpbGVOYW1lczogc3RyaW5nW10pID0+IFByb21pc2U8dm9pZD47XG5cbi8qKlxuICogSGVscGVycyBwYXNzZWQgdG8gaG9vayBmdW5jdGlvbnNcbiAqL1xuZXhwb3J0IGludGVyZmFjZSBIb29rQ29udGV4dCB7XG4gIC8qKlxuICAgKiBDYWxsYmFjayBmdW5jdGlvbiB0byByZXBsYWNlIHBsYWNlaG9sZGVycyBvbiBhcmJpdHJhcnkgZmlsZXNcbiAgICpcbiAgICogVGhpcyBtYWtlcyB0b2tlbiBzdWJzdGl0dXRpb24gYXZhaWxhYmxlIHRvIG5vbi1gLnRlbXBsYXRlYCBmaWxlcy5cbiAgICovXG4gIHJlYWRvbmx5IHN1YnN0aXR1dGVQbGFjZWhvbGRlcnNJbjogU3Vic3RpdHV0ZVBsYWNlaG9sZGVycztcblxuICAvKipcbiAgICogUmV0dXJuIGEgc2luZ2xlIHBsYWNlaG9sZGVyXG4gICAqL1xuICBwbGFjZWhvbGRlcihuYW1lOiBzdHJpbmcpOiBzdHJpbmc7XG59XG5cbmV4cG9ydCB0eXBlIEludm9rZUhvb2sgPSAodGFyZ2V0RGlyZWN0b3J5OiBzdHJpbmcsIGNvbnRleHQ6IEhvb2tDb250ZXh0KSA9PiBQcm9taXNlPHZvaWQ+O1xuXG5leHBvcnQgaW50ZXJmYWNlIEhvb2tUYXJnZXQge1xuICByZWFkb25seSB0YXJnZXREaXJlY3Rvcnk6IHN0cmluZztcbiAgcmVhZG9ubHkgdGVtcGxhdGVOYW1lOiBzdHJpbmc7XG4gIHJlYWRvbmx5IGxhbmd1YWdlOiBzdHJpbmc7XG59XG5cbi8qKlxuICogSW52b2tlIGhvb2tzIGZvciB0aGUgZ2l2ZW4gaW5pdCB0ZW1wbGF0ZVxuICpcbiAqIFNvbWV0aW1lcyB0ZW1wbGF0ZXMgbmVlZCBtb3JlIGNvbXBsZXggbG9naWMgdGhhbiBqdXN0IHJlcGxhY2luZyB0b2tlbnMuIEEgJ2hvb2snIGNhbiBiZVxuICogdXNlZCB0byBkbyBhZGRpdGlvbmFsIHByb2Nlc3Npbmcgb3RoZXIgdGhhbiBjb3B5aW5nIGZpbGVzLlxuICpcbiAqIEhvb2tzIHVzZWQgdG8gYmUgZGVmaW5lZCBleHRlcm5hbGx5IHRvIHRoZSBDTEksIGJ5IHJ1bm5pbmcgYXJiaXRyYXJpbHlcbiAqIHN1YnN0aXR1dGVkIHNoZWxsIHNjcmlwdHMgaW4gdGhlIHRhcmdldCBkaXJlY3RvcnkuXG4gKlxuICogSW4gcHJhY3RpY2UsIHRoZXkncmUgYWxsIFR5cGVTY3JpcHQgZmlsZXMgYW5kIGFsbCB0aGUgc2FtZSwgYW5kIHRoZSBkeW5hbWlzbVxuICogdGhhdCB0aGUgb3JpZ2luYWwgc29sdXRpb24gYWxsb3dlZCB3YXNuJ3QgdXNlZCBhdCBhbGwuIFdvcnNlLCBzaW5jZSB0aGUgQ0xJXG4gKiBpcyBub3cgYnVuZGxlZCB0aGUgaG9va3MgY2FuJ3QgZXZlbiByZXVzZSBjb2RlIGZyb20gdGhlIENMSSBsaWJyYXJpZXMgYXQgYWxsXG4gKiBhbnltb3JlLCBzbyBhbGwgc2hhcmVkIGNvZGUgd291bGQgaGF2ZSB0byBiZSBjb3B5L3Bhc3RlZC5cbiAqXG4gKiBCdW5kbGUgaG9va3MgYXMgYnVpbHQtaW5zIGludG8gdGhlIENMSSwgc28gdGhleSBnZXQgYnVuZGxlZCBhbmQgY2FuIHRha2UgYWR2YW50YWdlXG4gKiBvZiBhbGwgc2hhcmVkIGNvZGUuXG4gKi9cbmV4cG9ydCBhc3luYyBmdW5jdGlvbiBpbnZva2VCdWlsdGluSG9va3MoaW9IZWxwZXI6IElvSGVscGVyLCB0YXJnZXQ6IEhvb2tUYXJnZXQsIGNvbnRleHQ6IEhvb2tDb250ZXh0KSB7XG4gIHN3aXRjaCAodGFyZ2V0Lmxhbmd1YWdlKSB7XG4gICAgY2FzZSAnY3NoYXJwJzpcbiAgICAgIGlmIChbJ2FwcCcsICdzYW1wbGUtYXBwJ10uaW5jbHVkZXModGFyZ2V0LnRlbXBsYXRlTmFtZSkpIHtcbiAgICAgICAgcmV0dXJuIGRvdG5ldEFkZFByb2plY3QoaW9IZWxwZXIsIHRhcmdldC50YXJnZXREaXJlY3RvcnksIGNvbnRleHQpO1xuICAgICAgfVxuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICdmc2hhcnAnOlxuICAgICAgaWYgKFsnYXBwJywgJ3NhbXBsZS1hcHAnXS5pbmNsdWRlcyh0YXJnZXQudGVtcGxhdGVOYW1lKSkge1xuICAgICAgICByZXR1cm4gZG90bmV0QWRkUHJvamVjdChpb0hlbHBlciwgdGFyZ2V0LnRhcmdldERpcmVjdG9yeSwgY29udGV4dCwgJ2ZzcHJvaicpO1xuICAgICAgfVxuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICdweXRob24nOlxuICAgICAgLy8gV2UgY2FuJ3QgY2FsbCB0aGlzIGZpbGUgJ3JlcXVpcmVtZW50cy50ZW1wbGF0ZS50eHQnIGJlY2F1c2UgRGVwZW5kYWJvdCBuZWVkcyB0byBiZSBhYmxlIHRvIGZpbmQgaXQuXG4gICAgICAvLyBUaGVyZWZvcmUsIGtlZXAgdGhlIGluLXJlcG8gbmFtZSBidXQgc3RpbGwgc3Vic3RpdHV0ZSBwbGFjZWhvbGRlcnMuXG4gICAgICBhd2FpdCBjb250ZXh0LnN1YnN0aXR1dGVQbGFjZWhvbGRlcnNJbigncmVxdWlyZW1lbnRzLnR4dCcpO1xuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICdqYXZhJzpcbiAgICAgIC8vIFdlIGNhbid0IGNhbGwgdGhpcyBmaWxlICdwb20udGVtcGxhdGUueG1sJy4uLiBmb3IgdGhlIHNhbWUgcmVhc29uIGFzIFB5dGhvbiBhYm92ZS5cbiAgICAgIGF3YWl0IGNvbnRleHQuc3Vic3RpdHV0ZVBsYWNlaG9sZGVyc0luKCdwb20ueG1sJyk7XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgJ2phdmFzY3JpcHQnOlxuICAgIGNhc2UgJ3R5cGVzY3JpcHQnOlxuICAgICAgLy8gU2VlIGFib3ZlLCBidXQgZm9yICdwYWNrYWdlLmpzb24nLlxuICAgICAgYXdhaXQgY29udGV4dC5zdWJzdGl0dXRlUGxhY2Vob2xkZXJzSW4oJ3BhY2thZ2UuanNvbicpO1xuICB9XG59XG5cbmFzeW5jIGZ1bmN0aW9uIGRvdG5ldEFkZFByb2plY3QoaW9IZWxwZXI6IElvSGVscGVyLCB0YXJnZXREaXJlY3Rvcnk6IHN0cmluZywgY29udGV4dDogSG9va0NvbnRleHQsIGV4dCA9ICdjc3Byb2onKSB7XG4gIGNvbnN0IHBuYW1lID0gY29udGV4dC5wbGFjZWhvbGRlcignbmFtZS5QYXNjYWxDYXNlZCcpO1xuICBjb25zdCBzbG5QYXRoID0gcGF0aC5qb2luKHRhcmdldERpcmVjdG9yeSwgJ3NyYycsIGAke3BuYW1lfS5zbG5gKTtcbiAgY29uc3QgY3Nwcm9qUGF0aCA9IHBhdGguam9pbih0YXJnZXREaXJlY3RvcnksICdzcmMnLCBwbmFtZSwgYCR7cG5hbWV9LiR7ZXh0fWApO1xuXG4gIC8vIFdlIHJldHJ5IHRoaXMgY29tbWFuZCBhIGNvdXBsZSBvZiB0aW1lcy4gSXQgdXN1YWxseSBuZXZlciBmYWlscywgZXhjZXB0IG9uIENJIHdoZXJlXG4gIC8vIHdlIHNvbWV0aW1lcyBzZWU6XG4gIC8vXG4gIC8vICAgU3lzdGVtLklPLklPRXhjZXB0aW9uOiBUaGUgc3lzdGVtIGNhbm5vdCBvcGVuIHRoZSBkZXZpY2Ugb3IgZmlsZSBzcGVjaWZpZWQuIDogJ051R2V0LU1pZ3JhdGlvbnMnXG4gIC8vXG4gIC8vIFRoaXMgZXJyb3IgY2FuIGJlIGNhdXNlZCBieSBsYWNrIG9mIHBlcm1pc3Npb25zIG9uIGEgdGVtcG9yYXJ5IGRpcmVjdG9yeSxcbiAgLy8gYnV0IGluIG91ciBjYXNlIGl0J3MgaW50ZXJtaXR0ZW50IHNvIG15IGd1ZXNzIGlzIGl0IGlzIGNhdXNlZCBieSBtdWx0aXBsZVxuICAvLyBpbnZvY2F0aW9ucyBvZiB0aGUgLk5FVCBDTEkgcnVubmluZyBpbiBwYXJhbGxlbCwgYW5kIHRyYW1wbGluZyBvbiBlYWNoXG4gIC8vIG90aGVyIGNyZWF0aW5nIGEgTXV0ZXguIFRoZXJlIGlzIG5vIGZpeCwgYW5kIGl0IGlzIGFubm95aW5nbHkgYnJlYWtpbmcgb3VyXG4gIC8vIENJIHJlZ3VsYXJseS4gUmV0cnkgYSBjb3VwbGUgb2YgdGltZXMgdG8gaW5jcmVhc2UgcmVsaWFiaWxpdHkuXG4gIC8vXG4gIC8vIC0gaHR0cHM6Ly9naXRodWIuY29tL2RvdG5ldC9zZGsvaXNzdWVzLzQzNzUwXG4gIC8vIC0gaHR0cHM6Ly9naXRodWIuY29tL2RvdG5ldC9ydW50aW1lL2lzc3Vlcy84MDYxOVxuICAvLyAtIGh0dHBzOi8vZ2l0aHViLmNvbS9kb3RuZXQvcnVudGltZS9pc3N1ZXMvOTE5ODdcbiAgY29uc3QgTUFYX0FUVEVNUFRTID0gMztcbiAgZm9yIChsZXQgYXR0ZW1wdCA9IDE7IDsgYXR0ZW1wdCsrKSB7XG4gICAgdHJ5IHtcbiAgICAgIGF3YWl0IHNoZWxsKGlvSGVscGVyLCBbJ2RvdG5ldCcsICdzbG4nLCBzbG5QYXRoLCAnYWRkJywgY3Nwcm9qUGF0aF0pO1xuICAgICAgcmV0dXJuO1xuICAgIH0gY2F0Y2ggKGU6IGFueSkge1xuICAgICAgaWYgKGF0dGVtcHQgPT09IE1BWF9BVFRFTVBUUykge1xuICAgICAgICB0aHJvdyBuZXcgVG9vbGtpdEVycm9yKGBDb3VsZCBub3QgYWRkIHByb2plY3QgJHtwbmFtZX0uJHtleHR9IHRvIHNvbHV0aW9uICR7cG5hbWV9LnNsbi4gJHtmb3JtYXRFcnJvck1lc3NhZ2UoZSl9YCk7XG4gICAgICB9XG5cbiAgICAgIC8vIFNsZWVwIGZvciBhIGJpdCB0aGVuIHRyeSBhZ2FpblxuICAgICAgYXdhaXQgbmV3IFByb21pc2UocmVzb2x2ZSA9PiBzZXRUaW1lb3V0KHJlc29sdmUsIDEwMDApKTtcbiAgICB9XG4gIH1cbn1cbiJdfQ==