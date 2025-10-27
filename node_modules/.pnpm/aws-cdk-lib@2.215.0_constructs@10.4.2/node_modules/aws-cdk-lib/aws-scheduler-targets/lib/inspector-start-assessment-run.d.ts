import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IRole } from '../../aws-iam';
import { IAssessmentTemplate } from '../../aws-inspector';
import { IScheduleTarget } from '../../aws-scheduler';
/**
 * Use an Amazon Inspector as a target for AWS EventBridge Scheduler.
 */
export declare class InspectorStartAssessmentRun extends ScheduleTargetBase implements IScheduleTarget {
    constructor(template: IAssessmentTemplate, props?: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
