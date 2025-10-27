import { Construct } from 'constructs';
import { QueueProps, RedriveAllowPolicy } from './index';
export declare function validateQueueProps(scope: Construct, props: QueueProps): void;
export declare function validateRedriveAllowPolicy(scope: Construct, policy: RedriveAllowPolicy): void;
