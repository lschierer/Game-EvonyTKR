import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IRole } from '../../aws-iam';
import * as lambda from '../../aws-lambda';
import { IScheduleTarget } from '../../aws-scheduler';
/**
 * Use an AWS Lambda function as a target for AWS EventBridge Scheduler.
 */
export declare class LambdaInvoke extends ScheduleTargetBase implements IScheduleTarget {
    private readonly func;
    constructor(func: lambda.IFunction, props?: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
