import { ScheduleTargetBase, ScheduleTargetBaseProps } from './target';
import { IRole } from '../../aws-iam';
import { IScheduleTarget } from '../../aws-scheduler';
import { IStateMachine } from '../../aws-stepfunctions';
/**
 * Use an AWS Step function as a target for AWS EventBridge Scheduler.
 */
export declare class StepFunctionsStartExecution extends ScheduleTargetBase implements IScheduleTarget {
    private readonly stateMachine;
    constructor(stateMachine: IStateMachine, props: ScheduleTargetBaseProps);
    protected addTargetActionToRole(role: IRole): void;
}
