import { Construct } from 'constructs';
import { EnvironmentType } from './environment-type';
import { IResource, Resource, Size } from '../../core';
/**
 * Construction properties of a CodeBuild {@link Fleet}.
 */
export interface FleetProps {
    /**
     * The name of the Fleet.
     *
     * @default - CloudFormation generated name
     */
    readonly fleetName?: string;
    /**
     * The number of machines allocated to the compute ﬂeet.
     * Deﬁnes the number of builds that can run in parallel.
     *
     * Minimum value of 1.
     */
    readonly baseCapacity: number;
    /**
     * The instance type of the compute fleet.
     *
     * @see https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_codebuild.ComputeType.html
     */
    readonly computeType: FleetComputeType;
    /**
     * The build environment (operating system/architecture/accelerator) type
     * made available to projects using this fleet
     */
    readonly environmentType: EnvironmentType;
    /**
     * The compute configuration of the compute fleet.
     *
     * This is only required if `computeType` is set to ATTRIBUTE_BASED.
     *
     * @see https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment-reserved-capacity.types
     *
     * @default - do not specify compute configuration
     */
    readonly computeConfiguration?: ComputeConfiguration;
}
/**
 * The compute type of the fleet.
 */
export declare enum MachineType {
    /**
     * General purpose compute type.
     */
    GENERAL = "GENERAL",
    /**
     * Non-Volatile Memory Express (NVMe) storage optimized compute type.
     */
    NVME = "NVME"
}
/**
 * The compute configuration for the fleet.
 *
 *  Despite what the CloudFormation schema says, the numeric properties (disk, memory, vCpu) are not optional.
 *  An `undefined` value will cause the CloudFormation deployment to fail, e.g.
 *  > Cannot invoke "java.lang.Integer.intValue()" because the return value of "software.amazon.codebuild.fleet.ComputeConfiguration.getMemory()" is null
 * Therefore, these properties default value is set to 0.
 */
export interface ComputeConfiguration {
    /**
     * The amount of disk space of the instance type included in your fleet.
     *
     * @default - No requirement, the actual value will be based on the other selected configuration properties
     */
    readonly disk?: Size;
    /**
     * The machine type of the instance type included in your fleet.
     *
     * @default - No requirement, the actual value will be based on the other selected configuration properties
     */
    readonly machineType?: MachineType;
    /**
     * The amount of memory of the instance type included in your fleet.
     *
     * @default - No requirement, the actual value will be based on the other selected configuration properties
     */
    readonly memory?: Size;
    /**
     * The number of vCPUs of the instance type included in your fleet.
     *
     * @default - No requirement, the actual value will be based on the other selected configuration properties
     */
    readonly vCpu?: number;
}
/**
 * Represents a {@link Fleet} for a reserved capacity CodeBuild project.
 */
export interface IFleet extends IResource {
    /**
     * The ARN of the fleet.
     * @attribute
     */
    readonly fleetArn: string;
    /**
     * The name of the fleet.
     * @attribute
     */
    readonly fleetName: string;
    /**
     * The compute type of the fleet.
     *
     * @see https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_codebuild.ComputeType.html
     */
    readonly computeType: FleetComputeType;
    /**
     * The build environment (operating system/architecture/accelerator) type
     * made available to projects using this fleet
     */
    readonly environmentType: EnvironmentType;
}
/**
 * Fleet for a reserved capacity CodeBuild project.
 *
 * Fleets allow for process builds or tests to run immediately and reduces build durations,
 * by reserving compute resources for your projects.
 *
 * You will be charged for the resources in the fleet, even if they are idle.
 *
 * @see https://docs.aws.amazon.com/codebuild/latest/userguide/fleets.html
 */
export declare class Fleet extends Resource implements IFleet {
    /** Uniquely identifies this class. */
    static readonly PROPERTY_INJECTION_ID: string;
    /**
     * Creates a Fleet construct that represents an external fleet.
     *
     * @param scope The scope creating construct (usually `this`).
     * @param id The construct's id.
     * @param fleetArn The ARN of the fleet.
     */
    static fromFleetArn(scope: Construct, id: string, fleetArn: string): IFleet;
    /**
     * The ARN of the fleet.
     */
    readonly fleetArn: string;
    /**
     * The name of the fleet.
     */
    readonly fleetName: string;
    /**
     * The compute type of the fleet.
     *
     * @see https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_codebuild.ComputeType.html
     */
    readonly computeType: FleetComputeType;
    /**
     * The build environment (operating system/architecture/accelerator) type
     * made available to projects using this fleet
     */
    readonly environmentType: EnvironmentType;
    constructor(scope: Construct, id: string, props: FleetProps);
    private validatePositiveInteger;
}
/**
 * Fleet build machine compute type. Subset of Fleet compatible {@link ComputeType} values.
 *
 * The allocated memory, vCPU count and disk space of the build machine for a
 * given compute type are dependent on the environment type.
 * Some compute types may also not be available for all environment types.
 *
 * @see https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment.types
 */
export declare enum FleetComputeType {
    /**
     * Small compute type
     *
     * May not be available for all environment types, see
     * {@link https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment.types docs}
     * for more information.
     */
    SMALL = "BUILD_GENERAL1_SMALL",
    /**
     * Medium compute type
     *
     * May not be available for all environment types, see
     * {@link https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment.types docs}
     * for more information.
     **/
    MEDIUM = "BUILD_GENERAL1_MEDIUM",
    /** Large compute type */
    LARGE = "BUILD_GENERAL1_LARGE",
    /**
     * Extra Large compute type
     *
     * May not be available for all environment types, see
     * {@link https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment.types docs}
     * for more information.
     **/
    X_LARGE = "BUILD_GENERAL1_XLARGE",
    /**
     * Extra, Extra Large compute type
     *
     * May not be available for all environment types, see
     * {@link https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment.types docs}
     * for more information.
     **/
    X2_LARGE = "BUILD_GENERAL1_2XLARGE",
    /**
     * Specify the amount of vCPUs, memory, disk space, and the type of machine.
     *
     * AWS CodeBuild will select the cheapest instance that satisfies your specified attributes from `computeConfiguration`.
     *
     * @see https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html#environment-reserved-capacity.types
     */
    ATTRIBUTE_BASED = "ATTRIBUTE_BASED_COMPUTE"
}
