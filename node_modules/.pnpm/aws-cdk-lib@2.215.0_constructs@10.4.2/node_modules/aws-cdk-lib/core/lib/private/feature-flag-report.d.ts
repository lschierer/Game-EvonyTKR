import { IConstruct } from 'constructs';
import { CloudAssemblyBuilder } from '../../../cx-api';
/**
 * Iterate through all feature flags, retrieve the user's context,
 * and create a Feature Flag report.
 */
export declare function generateFeatureFlagReport(builder: CloudAssemblyBuilder, root: IConstruct): void;
