import { Construct } from 'constructs';
import { DatabaseClusterProps } from './cluster';
import { DatabaseInstanceProps } from './instance';
/**
 * Validates database instance properties
 */
export declare function validateDatabaseInstanceProps(scope: Construct, props: DatabaseInstanceProps): void;
/**
 * Validates database cluster properties
 */
export declare function validateDatabaseClusterProps(scope: Construct, props: DatabaseClusterProps): void;
