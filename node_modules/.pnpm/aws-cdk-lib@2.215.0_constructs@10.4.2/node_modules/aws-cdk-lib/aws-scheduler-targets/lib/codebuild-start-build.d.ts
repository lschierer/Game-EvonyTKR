import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IProject } from '../../aws-codebuild';
import { IRole } from '../../aws-iam';
import { IScheduleTarget } from '../../aws-scheduler';
/**
 * Use an AWS CodeBuild as a target for AWS EventBridge Scheduler.
 */
export declare class CodeBuildStartBuild extends ScheduleTargetBase implements IScheduleTarget {
    private readonly project;
    constructor(project: IProject, props?: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
