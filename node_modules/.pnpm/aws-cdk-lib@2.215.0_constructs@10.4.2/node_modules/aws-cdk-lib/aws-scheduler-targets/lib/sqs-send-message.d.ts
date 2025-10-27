import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IRole } from '../../aws-iam';
import { ISchedule, IScheduleTarget, ScheduleTargetConfig } from '../../aws-scheduler';
import * as sqs from '../../aws-sqs';
/**
 * Properties for a SQS Queue Target
 */
export interface SqsSendMessageProps extends ScheduleTargetBaseProps {
    /**
     * The FIFO message group ID to use as the target.
     *
     * This must be specified when the target is a FIFO queue. If you specify
     * a FIFO queue as a target, the queue must have content-based deduplication enabled.
     *
     * A length of `messageGroupId` must be between 1 and 128.
     *
     * @see https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-scheduler-schedule-sqsparameters.html#cfn-scheduler-schedule-sqsparameters-messagegroupid
     *
     * @default - no message group ID
     */
    readonly messageGroupId?: string;
}
/**
 * Use an Amazon SQS Queue as a target for AWS EventBridge Scheduler.
 */
export declare class SqsSendMessage extends ScheduleTargetBase implements IScheduleTarget {
    private readonly queue;
    private readonly props;
    constructor(queue: sqs.IQueue, props?: SqsSendMessageProps);
    protected addTargetActionToRole(role: IRole): void;
    protected bindBaseTargetConfig(_schedule: ISchedule): ScheduleTargetConfig;
}
