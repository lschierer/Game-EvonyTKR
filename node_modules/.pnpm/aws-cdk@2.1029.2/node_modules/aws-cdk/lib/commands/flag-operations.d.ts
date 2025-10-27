import type { FeatureFlag, Toolkit } from '@aws-cdk/toolkit-lib';
import type { IoHelper } from '../api-private';
import type { FlagsOptions } from '../cli/user-input';
interface FlagOperationsParams {
    flagData: FeatureFlag[];
    toolkit: Toolkit;
    ioHelper: IoHelper;
    /** User ran --recommended option */
    recommended?: boolean;
    /** User ran --all option */
    all?: boolean;
    /** User provided --value field */
    value?: string;
    /** User provided FLAGNAME field */
    flagName?: string[];
    /** User ran --default option */
    default?: boolean;
    /** User ran --unconfigured option */
    unconfigured?: boolean;
}
export declare function handleFlags(flagData: FeatureFlag[], ioHelper: IoHelper, options: FlagsOptions, toolkit: Toolkit): Promise<void>;
export declare function displayFlags(params: FlagOperationsParams): Promise<void>;
export {};
