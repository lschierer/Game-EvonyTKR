import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IRole } from '../../aws-iam';
import { IDeliveryStream } from '../../aws-kinesisfirehose';
import { IScheduleTarget } from '../../aws-scheduler';
/**
 * Use an Amazon Data Firehose as a target for AWS EventBridge Scheduler.
 */
export declare class FirehosePutRecord extends ScheduleTargetBase implements IScheduleTarget {
    private readonly deliveryStream;
    constructor(deliveryStream: IDeliveryStream, props?: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
