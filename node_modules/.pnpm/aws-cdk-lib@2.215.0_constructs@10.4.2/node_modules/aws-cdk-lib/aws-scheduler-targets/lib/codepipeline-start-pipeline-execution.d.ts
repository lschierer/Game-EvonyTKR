import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IPipeline } from '../../aws-codepipeline';
import { IRole } from '../../aws-iam';
import { IScheduleTarget } from '../../aws-scheduler';
/**
 * Use an AWS CodePipeline pipeline as a target for AWS EventBridge Scheduler.
 */
export declare class CodePipelineStartPipelineExecution extends ScheduleTargetBase implements IScheduleTarget {
    private readonly pipeline;
    constructor(pipeline: IPipeline, props?: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
