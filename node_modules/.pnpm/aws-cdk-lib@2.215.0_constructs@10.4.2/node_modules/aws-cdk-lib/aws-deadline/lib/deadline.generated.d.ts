import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a Farm.
 *
 * @stability experimental
 */
export interface IFarmRef extends constructs.IConstruct {
    /**
     * A reference to a Farm resource.
     */
    readonly farmRef: FarmReference;
}
/**
 * Creates a farm to allow space for queues and fleets.
 *
 * Farms are the space where the components of your renders gather and are pieced together in the cloud. Farms contain budgets and allow you to enforce permissions. Deadline Cloud farms are a useful container for large projects.
 *
 * @cloudformationResource AWS::Deadline::Farm
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html
 */
export declare class CfnFarm extends cdk.CfnResource implements cdk.IInspectable, IFarmRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnFarm from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnFarm;
    /**
     * The Amazon Resource Name (ARN) assigned to the farm.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The farm ID.
     *
     * @cloudformationAttribute FarmId
     */
    readonly attrFarmId: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * A description of the farm that helps identify what the farm is used for.
     */
    description?: string;
    /**
     * The display name of the farm.
     */
    displayName: string;
    /**
     * The ARN for the KMS key.
     */
    kmsKeyArn?: string;
    /**
     * The tags to add to your farm.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnFarmProps);
    get farmRef(): FarmReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnFarm`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html
 */
export interface CfnFarmProps {
    /**
     * A description of the farm that helps identify what the farm is used for.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html#cfn-deadline-farm-description
     */
    readonly description?: string;
    /**
     * The display name of the farm.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html#cfn-deadline-farm-displayname
     */
    readonly displayName: string;
    /**
     * The ARN for the KMS key.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html#cfn-deadline-farm-kmskeyarn
     */
    readonly kmsKeyArn?: string;
    /**
     * The tags to add to your farm.
     *
     * Each tag consists of a tag key and a tag value. Tag keys and values are both required, but tag values can be empty strings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-farm.html#cfn-deadline-farm-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Farm resource.
 *
 * @struct
 * @stability external
 */
export interface FarmReference {
    /**
     * The Arn of the Farm resource.
     */
    readonly farmArn: string;
}
/**
 * Indicates that this resource can be referenced as a Fleet.
 *
 * @stability experimental
 */
export interface IFleetRef extends constructs.IConstruct {
    /**
     * A reference to a Fleet resource.
     */
    readonly fleetRef: FleetReference;
}
/**
 * Creates a fleet.
 *
 * Fleets gather information relating to compute, or capacity, for renders within your farms. You can choose to manage your own capacity or opt to have fleets fully managed by Deadline Cloud.
 *
 * @cloudformationResource AWS::Deadline::Fleet
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html
 */
export declare class CfnFleet extends cdk.CfnResource implements cdk.IInspectable, IFleetRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnFleet from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnFleet;
    /**
     * The Amazon Resource Name (ARN) assigned to the fleet.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * @cloudformationAttribute Capabilities
     */
    readonly attrCapabilities: cdk.IResolvable;
    /**
     * The fleet ID.
     *
     * @cloudformationAttribute FleetId
     */
    readonly attrFleetId: string;
    /**
     * The status of the fleet.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * A message that communicates a suspended status of the fleet.
     *
     * @cloudformationAttribute StatusMessage
     */
    readonly attrStatusMessage: string;
    /**
     * The number of workers in the fleet summary.
     *
     * @cloudformationAttribute WorkerCount
     */
    readonly attrWorkerCount: number;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The configuration details for the fleet.
     */
    configuration: CfnFleet.FleetConfigurationProperty | cdk.IResolvable;
    /**
     * A description that helps identify what the fleet is used for.
     */
    description?: string;
    /**
     * The display name of the fleet summary to update.
     */
    displayName: string;
    /**
     * The farm ID.
     */
    farmId: string;
    /**
     * Provides a script that runs as a worker is starting up that you can use to provide additional configuration for workers in your fleet.
     */
    hostConfiguration?: CfnFleet.HostConfigurationProperty | cdk.IResolvable;
    /**
     * The maximum number of workers specified in the fleet.
     */
    maxWorkerCount: number;
    /**
     * The minimum number of workers in the fleet.
     */
    minWorkerCount?: number;
    /**
     * The IAM role that workers in the fleet use when processing jobs.
     */
    roleArn: string;
    /**
     * The tags to add to your fleet.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnFleetProps);
    get fleetRef(): FleetReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnFleet {
    /**
     * Fleet configuration details.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetconfiguration.html
     */
    interface FleetConfigurationProperty {
        /**
         * The customer managed fleets within a fleet configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetconfiguration.html#cfn-deadline-fleet-fleetconfiguration-customermanaged
         */
        readonly customerManaged?: CfnFleet.CustomerManagedFleetConfigurationProperty | cdk.IResolvable;
        /**
         * The service managed Amazon EC2 instances for a fleet configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetconfiguration.html#cfn-deadline-fleet-fleetconfiguration-servicemanagedec2
         */
        readonly serviceManagedEc2?: cdk.IResolvable | CfnFleet.ServiceManagedEc2FleetConfigurationProperty;
    }
    /**
     * The details of a customer managed fleet configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedfleetconfiguration.html
     */
    interface CustomerManagedFleetConfigurationProperty {
        /**
         * The AWS Auto Scaling mode for the customer managed fleet configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedfleetconfiguration.html#cfn-deadline-fleet-customermanagedfleetconfiguration-mode
         */
        readonly mode: string;
        /**
         * The storage profile ID.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedfleetconfiguration.html#cfn-deadline-fleet-customermanagedfleetconfiguration-storageprofileid
         */
        readonly storageProfileId?: string;
        /**
         * Specifies whether tags associated with a fleet are attached to workers when the worker is launched.
         *
         * When the `tagPropagationMode` is set to `PROPAGATE_TAGS_TO_WORKERS_AT_LAUNCH` any tag associated with a fleet is attached to workers when they launch. If the tags for a fleet change, the tags associated with running workers *do not* change.
         *
         * If you don't specify `tagPropagationMode` , the default is `NO_PROPAGATION` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedfleetconfiguration.html#cfn-deadline-fleet-customermanagedfleetconfiguration-tagpropagationmode
         */
        readonly tagPropagationMode?: string;
        /**
         * The worker capabilities for a customer managed fleet configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedfleetconfiguration.html#cfn-deadline-fleet-customermanagedfleetconfiguration-workercapabilities
         */
        readonly workerCapabilities: CfnFleet.CustomerManagedWorkerCapabilitiesProperty | cdk.IResolvable;
    }
    /**
     * The worker capabilities for a customer managed workflow.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html
     */
    interface CustomerManagedWorkerCapabilitiesProperty {
        /**
         * The range of the accelerator.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-acceleratorcount
         */
        readonly acceleratorCount?: CfnFleet.AcceleratorCountRangeProperty | cdk.IResolvable;
        /**
         * The total memory (MiB) for the customer managed worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-acceleratortotalmemorymib
         */
        readonly acceleratorTotalMemoryMiB?: CfnFleet.AcceleratorTotalMemoryMiBRangeProperty | cdk.IResolvable;
        /**
         * The accelerator types for the customer managed worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-acceleratortypes
         */
        readonly acceleratorTypes?: Array<string>;
        /**
         * The CPU architecture type for the customer managed worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-cpuarchitecturetype
         */
        readonly cpuArchitectureType: string;
        /**
         * Custom requirement ranges for customer managed worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-customamounts
         */
        readonly customAmounts?: Array<CfnFleet.FleetAmountCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * Custom attributes for the customer manged worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-customattributes
         */
        readonly customAttributes?: Array<CfnFleet.FleetAttributeCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * The memory (MiB).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-memorymib
         */
        readonly memoryMiB: cdk.IResolvable | CfnFleet.MemoryMiBRangeProperty;
        /**
         * The operating system (OS) family.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-osfamily
         */
        readonly osFamily: string;
        /**
         * The vCPU count for the customer manged worker capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-customermanagedworkercapabilities.html#cfn-deadline-fleet-customermanagedworkercapabilities-vcpucount
         */
        readonly vCpuCount: cdk.IResolvable | CfnFleet.VCpuCountRangeProperty;
    }
    /**
     * The allowable range of vCPU processing power for the fleet.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-vcpucountrange.html
     */
    interface VCpuCountRangeProperty {
        /**
         * The maximum amount of vCPU.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-vcpucountrange.html#cfn-deadline-fleet-vcpucountrange-max
         */
        readonly max?: number;
        /**
         * The minimum amount of vCPU.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-vcpucountrange.html#cfn-deadline-fleet-vcpucountrange-min
         */
        readonly min: number;
    }
    /**
     * The range of memory in MiB.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-memorymibrange.html
     */
    interface MemoryMiBRangeProperty {
        /**
         * The maximum amount of memory (in MiB).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-memorymibrange.html#cfn-deadline-fleet-memorymibrange-max
         */
        readonly max?: number;
        /**
         * The minimum amount of memory (in MiB).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-memorymibrange.html#cfn-deadline-fleet-memorymibrange-min
         */
        readonly min: number;
    }
    /**
     * Defines the maximum and minimum number of GPU accelerators required for a worker instance..
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcountrange.html
     */
    interface AcceleratorCountRangeProperty {
        /**
         * The maximum number of GPU accelerators in the worker host.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcountrange.html#cfn-deadline-fleet-acceleratorcountrange-max
         */
        readonly max?: number;
        /**
         * The minimum number of GPU accelerators in the worker host.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcountrange.html#cfn-deadline-fleet-acceleratorcountrange-min
         */
        readonly min: number;
    }
    /**
     * Defines the maximum and minimum amount of memory, in MiB, to use for the accelerator.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratortotalmemorymibrange.html
     */
    interface AcceleratorTotalMemoryMiBRangeProperty {
        /**
         * The maximum amount of memory to use for the accelerator, measured in MiB.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratortotalmemorymibrange.html#cfn-deadline-fleet-acceleratortotalmemorymibrange-max
         */
        readonly max?: number;
        /**
         * The minimum amount of memory to use for the accelerator, measured in MiB.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratortotalmemorymibrange.html#cfn-deadline-fleet-acceleratortotalmemorymibrange-min
         */
        readonly min: number;
    }
    /**
     * The fleet amount and attribute capabilities.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetamountcapability.html
     */
    interface FleetAmountCapabilityProperty {
        /**
         * The maximum amount of the fleet worker capability.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetamountcapability.html#cfn-deadline-fleet-fleetamountcapability-max
         */
        readonly max?: number;
        /**
         * The minimum amount of fleet worker capability.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetamountcapability.html#cfn-deadline-fleet-fleetamountcapability-min
         */
        readonly min: number;
        /**
         * The name of the fleet capability.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetamountcapability.html#cfn-deadline-fleet-fleetamountcapability-name
         */
        readonly name: string;
    }
    /**
     * Defines the fleet's capability name, minimum, and maximum.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetattributecapability.html
     */
    interface FleetAttributeCapabilityProperty {
        /**
         * The name of the fleet attribute capability for the worker.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetattributecapability.html#cfn-deadline-fleet-fleetattributecapability-name
         */
        readonly name: string;
        /**
         * The number of fleet attribute capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetattributecapability.html#cfn-deadline-fleet-fleetattributecapability-values
         */
        readonly values: Array<string>;
    }
    /**
     * The configuration details for a service managed Amazon EC2 fleet.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2fleetconfiguration.html
     */
    interface ServiceManagedEc2FleetConfigurationProperty {
        /**
         * The Amazon EC2 instance capabilities.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2fleetconfiguration.html#cfn-deadline-fleet-servicemanagedec2fleetconfiguration-instancecapabilities
         */
        readonly instanceCapabilities: cdk.IResolvable | CfnFleet.ServiceManagedEc2InstanceCapabilitiesProperty;
        /**
         * The Amazon EC2 market type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2fleetconfiguration.html#cfn-deadline-fleet-servicemanagedec2fleetconfiguration-instancemarketoptions
         */
        readonly instanceMarketOptions: cdk.IResolvable | CfnFleet.ServiceManagedEc2InstanceMarketOptionsProperty;
        /**
         * The storage profile ID.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2fleetconfiguration.html#cfn-deadline-fleet-servicemanagedec2fleetconfiguration-storageprofileid
         */
        readonly storageProfileId?: string;
        /**
         * The VPC configuration details for a service managed Amazon EC2 fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2fleetconfiguration.html#cfn-deadline-fleet-servicemanagedec2fleetconfiguration-vpcconfiguration
         */
        readonly vpcConfiguration?: cdk.IResolvable | CfnFleet.VpcConfigurationProperty;
    }
    /**
     * The Amazon EC2 instance capabilities.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html
     */
    interface ServiceManagedEc2InstanceCapabilitiesProperty {
        /**
         * Describes the GPU accelerator capabilities required for worker host instances in this fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-acceleratorcapabilities
         */
        readonly acceleratorCapabilities?: CfnFleet.AcceleratorCapabilitiesProperty | cdk.IResolvable;
        /**
         * The allowable Amazon EC2 instance types.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-allowedinstancetypes
         */
        readonly allowedInstanceTypes?: Array<string>;
        /**
         * The CPU architecture type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-cpuarchitecturetype
         */
        readonly cpuArchitectureType: string;
        /**
         * The custom capability amounts to require for instances in this fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-customamounts
         */
        readonly customAmounts?: Array<CfnFleet.FleetAmountCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * The custom capability attributes to require for instances in this fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-customattributes
         */
        readonly customAttributes?: Array<CfnFleet.FleetAttributeCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * The instance types to exclude from the fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-excludedinstancetypes
         */
        readonly excludedInstanceTypes?: Array<string>;
        /**
         * The memory, as MiB, for the Amazon EC2 instance type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-memorymib
         */
        readonly memoryMiB: cdk.IResolvable | CfnFleet.MemoryMiBRangeProperty;
        /**
         * The operating system (OS) family.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-osfamily
         */
        readonly osFamily: string;
        /**
         * The root EBS volume.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-rootebsvolume
         */
        readonly rootEbsVolume?: CfnFleet.Ec2EbsVolumeProperty | cdk.IResolvable;
        /**
         * The amount of vCPU to require for instances in this fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancecapabilities.html#cfn-deadline-fleet-servicemanagedec2instancecapabilities-vcpucount
         */
        readonly vCpuCount: cdk.IResolvable | CfnFleet.VCpuCountRangeProperty;
    }
    /**
     * Specifies the EBS volume.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-ec2ebsvolume.html
     */
    interface Ec2EbsVolumeProperty {
        /**
         * The IOPS per volume.
         *
         * @default - 3000
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-ec2ebsvolume.html#cfn-deadline-fleet-ec2ebsvolume-iops
         */
        readonly iops?: number;
        /**
         * The EBS volume size in GiB.
         *
         * @default - 250
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-ec2ebsvolume.html#cfn-deadline-fleet-ec2ebsvolume-sizegib
         */
        readonly sizeGiB?: number;
        /**
         * The throughput per volume in MiB.
         *
         * @default - 125
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-ec2ebsvolume.html#cfn-deadline-fleet-ec2ebsvolume-throughputmib
         */
        readonly throughputMiB?: number;
    }
    /**
     * Provides information about the GPU accelerators used for jobs processed by a fleet.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcapabilities.html
     */
    interface AcceleratorCapabilitiesProperty {
        /**
         * The number of GPU accelerators specified for worker hosts in this fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcapabilities.html#cfn-deadline-fleet-acceleratorcapabilities-count
         */
        readonly count?: CfnFleet.AcceleratorCountRangeProperty | cdk.IResolvable;
        /**
         * A list of accelerator capabilities requested for this fleet.
         *
         * Only Amazon Elastic Compute Cloud instances that provide these capabilities will be used. For example, if you specify both L4 and T4 chips, Deadline Cloud will use Amazon EC2 instances that have either the L4 or the T4 chip installed.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorcapabilities.html#cfn-deadline-fleet-acceleratorcapabilities-selections
         */
        readonly selections: Array<CfnFleet.AcceleratorSelectionProperty | cdk.IResolvable> | cdk.IResolvable;
    }
    /**
     * Describes a specific GPU accelerator required for an Amazon Elastic Compute Cloud worker host.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorselection.html
     */
    interface AcceleratorSelectionProperty {
        /**
         * The name of the chip used by the GPU accelerator.
         *
         * If you specify `l4` as the name of the accelerator, you must specify `latest` or `grid:r570` as the runtime.
         *
         * The available GPU accelerators are:
         *
         * - `t4` - NVIDIA T4 Tensor Core GPU
         * - `a10g` - NVIDIA A10G Tensor Core GPU
         * - `l4` - NVIDIA L4 Tensor Core GPU
         * - `l40s` - NVIDIA L40S Tensor Core GPU
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorselection.html#cfn-deadline-fleet-acceleratorselection-name
         */
        readonly name: string;
        /**
         * Specifies the runtime driver to use for the GPU accelerator. You must use the same runtime for all GPUs.
         *
         * You can choose from the following runtimes:
         *
         * - `latest` - Use the latest runtime available for the chip. If you specify `latest` and a new version of the runtime is released, the new version of the runtime is used.
         * - `grid:r570` - [NVIDIA vGPU software 18](https://docs.aws.amazon.com/https://docs.nvidia.com/vgpu/18.0/index.html)
         * - `grid:r535` - [NVIDIA vGPU software 16](https://docs.aws.amazon.com/https://docs.nvidia.com/vgpu/16.0/index.html)
         *
         * If you don't specify a runtime, Deadline Cloud uses `latest` as the default. However, if you have multiple accelerators and specify `latest` for some and leave others blank, Deadline Cloud raises an exception.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-acceleratorselection.html#cfn-deadline-fleet-acceleratorselection-runtime
         */
        readonly runtime?: string;
    }
    /**
     * The details of the Amazon EC2 instance market options for a service managed fleet.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancemarketoptions.html
     */
    interface ServiceManagedEc2InstanceMarketOptionsProperty {
        /**
         * The Amazon EC2 instance type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-servicemanagedec2instancemarketoptions.html#cfn-deadline-fleet-servicemanagedec2instancemarketoptions-type
         */
        readonly type: string;
    }
    /**
     * The configuration options for a service managed fleet's VPC.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-vpcconfiguration.html
     */
    interface VpcConfigurationProperty {
        /**
         * The ARNs of the VPC Lattice resource configurations attached to the fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-vpcconfiguration.html#cfn-deadline-fleet-vpcconfiguration-resourceconfigurationarns
         */
        readonly resourceConfigurationArns?: Array<string>;
    }
    /**
     * Provides a script that runs as a worker is starting up that you can use to provide additional configuration for workers in your fleet.
     *
     * To remove a script from a fleet, use the [UpdateFleet](https://docs.aws.amazon.com/deadline-cloud/latest/APIReference/API_UpdateFleet.html) operation with the `hostConfiguration` `scriptBody` parameter set to an empty string ("").
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-hostconfiguration.html
     */
    interface HostConfigurationProperty {
        /**
         * The text of the script that runs as a worker is starting up that you can use to provide additional configuration for workers in your fleet.
         *
         * The script runs after a worker enters the `STARTING` state and before the worker processes tasks.
         *
         * For more information about using the script, see [Run scripts as an administrator to configure workers](https://docs.aws.amazon.com/deadline-cloud/latest/developerguide/smf-admin.html) in the *Deadline Cloud Developer Guide* .
         *
         * > The script runs as an administrative user ( `sudo root` on Linux, as an Administrator on Windows).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-hostconfiguration.html#cfn-deadline-fleet-hostconfiguration-scriptbody
         */
        readonly scriptBody: string;
        /**
         * The maximum time that the host configuration can run.
         *
         * If the timeout expires, the worker enters the `NOT RESPONDING` state and shuts down. You are charged for the time that the worker is running the host configuration script.
         *
         * > You should configure your fleet for a maximum of one worker while testing your host configuration script to avoid starting additional workers.
         *
         * The default is 300 seconds (5 minutes).
         *
         * @default - 300
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-hostconfiguration.html#cfn-deadline-fleet-hostconfiguration-scripttimeoutseconds
         */
        readonly scriptTimeoutSeconds?: number;
    }
    /**
     * The amounts and attributes of fleets.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetcapabilities.html
     */
    interface FleetCapabilitiesProperty {
        /**
         * Amount capabilities of the fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetcapabilities.html#cfn-deadline-fleet-fleetcapabilities-amounts
         */
        readonly amounts?: Array<CfnFleet.FleetAmountCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * Attribute capabilities of the fleet.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-fleet-fleetcapabilities.html#cfn-deadline-fleet-fleetcapabilities-attributes
         */
        readonly attributes?: Array<CfnFleet.FleetAttributeCapabilityProperty | cdk.IResolvable> | cdk.IResolvable;
    }
}
/**
 * Properties for defining a `CfnFleet`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html
 */
export interface CfnFleetProps {
    /**
     * The configuration details for the fleet.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-configuration
     */
    readonly configuration: CfnFleet.FleetConfigurationProperty | cdk.IResolvable;
    /**
     * A description that helps identify what the fleet is used for.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-description
     */
    readonly description?: string;
    /**
     * The display name of the fleet summary to update.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-displayname
     */
    readonly displayName: string;
    /**
     * The farm ID.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-farmid
     */
    readonly farmId: string;
    /**
     * Provides a script that runs as a worker is starting up that you can use to provide additional configuration for workers in your fleet.
     *
     * To remove a script from a fleet, use the [UpdateFleet](https://docs.aws.amazon.com/deadline-cloud/latest/APIReference/API_UpdateFleet.html) operation with the `hostConfiguration` `scriptBody` parameter set to an empty string ("").
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-hostconfiguration
     */
    readonly hostConfiguration?: CfnFleet.HostConfigurationProperty | cdk.IResolvable;
    /**
     * The maximum number of workers specified in the fleet.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-maxworkercount
     */
    readonly maxWorkerCount: number;
    /**
     * The minimum number of workers in the fleet.
     *
     * @default - 0
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-minworkercount
     */
    readonly minWorkerCount?: number;
    /**
     * The IAM role that workers in the fleet use when processing jobs.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-rolearn
     */
    readonly roleArn: string;
    /**
     * The tags to add to your fleet.
     *
     * Each tag consists of a tag key and a tag value. Tag keys and values are both required, but tag values can be empty strings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-fleet.html#cfn-deadline-fleet-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Fleet resource.
 *
 * @struct
 * @stability external
 */
export interface FleetReference {
    /**
     * The Arn of the Fleet resource.
     */
    readonly fleetArn: string;
}
/**
 * Indicates that this resource can be referenced as a LicenseEndpoint.
 *
 * @stability experimental
 */
export interface ILicenseEndpointRef extends constructs.IConstruct {
    /**
     * A reference to a LicenseEndpoint resource.
     */
    readonly licenseEndpointRef: LicenseEndpointReference;
}
/**
 * Creates a license endpoint to integrate your various licensed software used for rendering on Deadline Cloud.
 *
 * @cloudformationResource AWS::Deadline::LicenseEndpoint
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html
 */
export declare class CfnLicenseEndpoint extends cdk.CfnResource implements cdk.IInspectable, ILicenseEndpointRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnLicenseEndpoint from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnLicenseEndpoint;
    /**
     * The Amazon Resource Name (ARN) of the license endpoint.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The DNS name of the license server endpoint.
     *
     * @cloudformationAttribute DnsName
     */
    readonly attrDnsName: string;
    /**
     * The license endpoint ID.
     *
     * @cloudformationAttribute LicenseEndpointId
     */
    readonly attrLicenseEndpointId: string;
    /**
     * The status of the license endpoint.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The status message of the license endpoint.
     *
     * @cloudformationAttribute StatusMessage
     */
    readonly attrStatusMessage: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The identifier of the Amazon EC2 security group that controls access to the license endpoint.
     */
    securityGroupIds: Array<string>;
    /**
     * Identifies the VPC subnets that can connect to a license endpoint.
     */
    subnetIds: Array<string>;
    /**
     * The tags to add to your license endpoint.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * The VPC (virtual private cloud) ID associated with the license endpoint.
     */
    vpcId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnLicenseEndpointProps);
    get licenseEndpointRef(): LicenseEndpointReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnLicenseEndpoint`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html
 */
export interface CfnLicenseEndpointProps {
    /**
     * The identifier of the Amazon EC2 security group that controls access to the license endpoint.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html#cfn-deadline-licenseendpoint-securitygroupids
     */
    readonly securityGroupIds: Array<string>;
    /**
     * Identifies the VPC subnets that can connect to a license endpoint.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html#cfn-deadline-licenseendpoint-subnetids
     */
    readonly subnetIds: Array<string>;
    /**
     * The tags to add to your license endpoint.
     *
     * Each tag consists of a tag key and a tag value. Tag keys and values are both required, but tag values can be empty strings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html#cfn-deadline-licenseendpoint-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The VPC (virtual private cloud) ID associated with the license endpoint.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-licenseendpoint.html#cfn-deadline-licenseendpoint-vpcid
     */
    readonly vpcId: string;
}
/**
 * A reference to a LicenseEndpoint resource.
 *
 * @struct
 * @stability external
 */
export interface LicenseEndpointReference {
    /**
     * The Arn of the LicenseEndpoint resource.
     */
    readonly licenseEndpointArn: string;
}
/**
 * Indicates that this resource can be referenced as a Limit.
 *
 * @stability experimental
 */
export interface ILimitRef extends constructs.IConstruct {
    /**
     * A reference to a Limit resource.
     */
    readonly limitRef: LimitReference;
}
/**
 * Creates a limit that manages the distribution of shared resources, such as floating licenses.
 *
 * A limit can throttle work assignments, help manage workloads, and track current usage. Before you use a limit, you must associate the limit with one or more queues.
 *
 * You must add the `amountRequirementName` to a step in a job template to declare the limit requirement.
 *
 * @cloudformationResource AWS::Deadline::Limit
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html
 */
export declare class CfnLimit extends cdk.CfnResource implements cdk.IInspectable, ILimitRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnLimit from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnLimit;
    /**
     * The number of resources from the limit that are being used by jobs. The result is delayed and may not be the count at the time that you called the operation.
     *
     * @cloudformationAttribute CurrentCount
     */
    readonly attrCurrentCount: number;
    /**
     * The unique identifier of the limit.
     *
     * @cloudformationAttribute LimitId
     */
    readonly attrLimitId: string;
    /**
     * The value that you specify as the `name` in the `amounts` field of the `hostRequirements` in a step of a job template to declare the limit requirement.
     */
    amountRequirementName: string;
    /**
     * A description of the limit. A clear description helps you identify the purpose of the limit.
     */
    description?: string;
    /**
     * The name of the limit used in lists to identify the limit.
     */
    displayName: string;
    /**
     * The unique identifier of the farm that contains the limit.
     */
    farmId: string;
    /**
     * The maximum number of resources constrained by this limit.
     */
    maxCount: number;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnLimitProps);
    get limitRef(): LimitReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnLimit`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html
 */
export interface CfnLimitProps {
    /**
     * The value that you specify as the `name` in the `amounts` field of the `hostRequirements` in a step of a job template to declare the limit requirement.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html#cfn-deadline-limit-amountrequirementname
     */
    readonly amountRequirementName: string;
    /**
     * A description of the limit. A clear description helps you identify the purpose of the limit.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html#cfn-deadline-limit-description
     */
    readonly description?: string;
    /**
     * The name of the limit used in lists to identify the limit.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html#cfn-deadline-limit-displayname
     */
    readonly displayName: string;
    /**
     * The unique identifier of the farm that contains the limit.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html#cfn-deadline-limit-farmid
     */
    readonly farmId: string;
    /**
     * The maximum number of resources constrained by this limit.
     *
     * When all of the resources are in use, steps that require the limit won't be scheduled until the resource is available.
     *
     * The `maxValue` must not be 0. If the value is -1, there is no restriction on the number of resources that can be acquired for this limit.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-limit.html#cfn-deadline-limit-maxcount
     */
    readonly maxCount: number;
}
/**
 * A reference to a Limit resource.
 *
 * @struct
 * @stability external
 */
export interface LimitReference {
    /**
     * The FarmId of the Limit resource.
     */
    readonly farmId: string;
    /**
     * The LimitId of the Limit resource.
     */
    readonly limitId: string;
}
/**
 * Indicates that this resource can be referenced as a MeteredProduct.
 *
 * @stability experimental
 */
export interface IMeteredProductRef extends constructs.IConstruct {
    /**
     * A reference to a MeteredProduct resource.
     */
    readonly meteredProductRef: MeteredProductReference;
}
/**
 * Adds a metered product.
 *
 * @cloudformationResource AWS::Deadline::MeteredProduct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-meteredproduct.html
 */
export declare class CfnMeteredProduct extends cdk.CfnResource implements cdk.IInspectable, IMeteredProductRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnMeteredProduct from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnMeteredProduct;
    /**
     * The Amazon Resource Name (ARN) of the metered product.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The family to which the metered product belongs.
     *
     * @cloudformationAttribute Family
     */
    readonly attrFamily: string;
    /**
     * The port on which the metered product should run.
     *
     * @cloudformationAttribute Port
     */
    readonly attrPort: number;
    /**
     * The vendor.
     *
     * @cloudformationAttribute Vendor
     */
    readonly attrVendor: string;
    /**
     * The Amazon EC2 identifier of the license endpoint.
     */
    licenseEndpointId?: string;
    /**
     * The product ID.
     */
    productId?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnMeteredProductProps);
    get meteredProductRef(): MeteredProductReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnMeteredProduct`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-meteredproduct.html
 */
export interface CfnMeteredProductProps {
    /**
     * The Amazon EC2 identifier of the license endpoint.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-meteredproduct.html#cfn-deadline-meteredproduct-licenseendpointid
     */
    readonly licenseEndpointId?: string;
    /**
     * The product ID.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-meteredproduct.html#cfn-deadline-meteredproduct-productid
     */
    readonly productId?: string;
}
/**
 * A reference to a MeteredProduct resource.
 *
 * @struct
 * @stability external
 */
export interface MeteredProductReference {
    /**
     * The Arn of the MeteredProduct resource.
     */
    readonly meteredProductArn: string;
}
/**
 * Indicates that this resource can be referenced as a Monitor.
 *
 * @stability experimental
 */
export interface IMonitorRef extends constructs.IConstruct {
    /**
     * A reference to a Monitor resource.
     */
    readonly monitorRef: MonitorReference;
}
/**
 * Creates an AWS Deadline Cloud monitor that you can use to view your farms, queues, and fleets.
 *
 * After you submit a job, you can track the progress of the tasks and steps that make up the job, and then download the job's results.
 *
 * @cloudformationResource AWS::Deadline::Monitor
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html
 */
export declare class CfnMonitor extends cdk.CfnResource implements cdk.IInspectable, IMonitorRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnMonitor from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnMonitor;
    /**
     * The Amazon Resource Name (ARN) of the monitor.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The Amazon Resource Name (ARN) that the IAM Identity Center assigned to the monitor when it was created.
     *
     * @cloudformationAttribute IdentityCenterApplicationArn
     */
    readonly attrIdentityCenterApplicationArn: string;
    /**
     * The unique identifier for the monitor.
     *
     * @cloudformationAttribute MonitorId
     */
    readonly attrMonitorId: string;
    /**
     * The complete URL of the monitor. The full URL of the monitor is subdomain.Region.deadlinecloud.amazonaws.com.
     *
     * @cloudformationAttribute Url
     */
    readonly attrUrl: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The name of the monitor that displays on the Deadline Cloud console.
     */
    displayName: string;
    /**
     * The Amazon Resource Name (ARN) of the IAM Identity Center instance responsible for authenticating monitor users.
     */
    identityCenterInstanceArn: string;
    /**
     * The Amazon Resource Name (ARN) of the IAM role for the monitor.
     */
    roleArn: string;
    /**
     * The subdomain used for the monitor URL.
     */
    subdomain: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnMonitorProps);
    get monitorRef(): MonitorReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnMonitor`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html
 */
export interface CfnMonitorProps {
    /**
     * The name of the monitor that displays on the Deadline Cloud console.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html#cfn-deadline-monitor-displayname
     */
    readonly displayName: string;
    /**
     * The Amazon Resource Name (ARN) of the IAM Identity Center instance responsible for authenticating monitor users.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html#cfn-deadline-monitor-identitycenterinstancearn
     */
    readonly identityCenterInstanceArn: string;
    /**
     * The Amazon Resource Name (ARN) of the IAM role for the monitor.
     *
     * Users of the monitor use this role to access Deadline Cloud resources.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html#cfn-deadline-monitor-rolearn
     */
    readonly roleArn: string;
    /**
     * The subdomain used for the monitor URL.
     *
     * The full URL of the monitor is subdomain.Region.deadlinecloud.amazonaws.com.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html#cfn-deadline-monitor-subdomain
     */
    readonly subdomain: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-monitor.html#cfn-deadline-monitor-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Monitor resource.
 *
 * @struct
 * @stability external
 */
export interface MonitorReference {
    /**
     * The Arn of the Monitor resource.
     */
    readonly monitorArn: string;
}
/**
 * Indicates that this resource can be referenced as a Queue.
 *
 * @stability experimental
 */
export interface IQueueRef extends constructs.IConstruct {
    /**
     * A reference to a Queue resource.
     */
    readonly queueRef: QueueReference;
}
/**
 * Creates a queue to coordinate the order in which jobs run on a farm.
 *
 * A queue can also specify where to pull resources and indicate where to output completed jobs.
 *
 * @cloudformationResource AWS::Deadline::Queue
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html
 */
export declare class CfnQueue extends cdk.CfnResource implements cdk.IInspectable, IQueueRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnQueue from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnQueue;
    /**
     * The Amazon Resource Name (ARN) of the queue.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The queue ID.
     *
     * @cloudformationAttribute QueueId
     */
    readonly attrQueueId: string;
    /**
     * The identifiers of the storage profiles that this queue can use to share assets between workers using different operating systems.
     */
    allowedStorageProfileIds?: Array<string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The default action taken on a queue summary if a budget wasn't configured.
     */
    defaultBudgetAction?: string;
    /**
     * A description of the queue that helps identify what the queue is used for.
     */
    description?: string;
    /**
     * The display name of the queue summary to update.
     */
    displayName: string;
    /**
     * The farm ID.
     */
    farmId: string;
    /**
     * The job attachment settings.
     */
    jobAttachmentSettings?: cdk.IResolvable | CfnQueue.JobAttachmentSettingsProperty;
    /**
     * Identifies the user for a job.
     */
    jobRunAsUser?: cdk.IResolvable | CfnQueue.JobRunAsUserProperty;
    /**
     * The file system location that the queue uses.
     */
    requiredFileSystemLocationNames?: Array<string>;
    /**
     * The Amazon Resource Name (ARN) of the IAM role that workers use when running jobs in this queue.
     */
    roleArn?: string;
    /**
     * The tags to add to your queue.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnQueueProps);
    get queueRef(): QueueReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnQueue {
    /**
     * The job attachment settings.
     *
     * These are the Amazon S3 bucket name and the Amazon S3 prefix.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobattachmentsettings.html
     */
    interface JobAttachmentSettingsProperty {
        /**
         * The root prefix.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobattachmentsettings.html#cfn-deadline-queue-jobattachmentsettings-rootprefix
         */
        readonly rootPrefix: string;
        /**
         * The Amazon S3 bucket name.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobattachmentsettings.html#cfn-deadline-queue-jobattachmentsettings-s3bucketname
         */
        readonly s3BucketName: string;
    }
    /**
     * Identifies the user for a job.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobrunasuser.html
     */
    interface JobRunAsUserProperty {
        /**
         * The user and group that the jobs in the queue run as.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobrunasuser.html#cfn-deadline-queue-jobrunasuser-posix
         */
        readonly posix?: cdk.IResolvable | CfnQueue.PosixUserProperty;
        /**
         * Specifies whether the job should run using the queue's system user or if the job should run using the worker agent system user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobrunasuser.html#cfn-deadline-queue-jobrunasuser-runas
         */
        readonly runAs: string;
        /**
         * Identifies a Microsoft Windows user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-jobrunasuser.html#cfn-deadline-queue-jobrunasuser-windows
         */
        readonly windows?: cdk.IResolvable | CfnQueue.WindowsUserProperty;
    }
    /**
     * The POSIX user.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-posixuser.html
     */
    interface PosixUserProperty {
        /**
         * The name of the POSIX user's group.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-posixuser.html#cfn-deadline-queue-posixuser-group
         */
        readonly group: string;
        /**
         * The name of the POSIX user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-posixuser.html#cfn-deadline-queue-posixuser-user
         */
        readonly user: string;
    }
    /**
     * The Windows user details.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-windowsuser.html
     */
    interface WindowsUserProperty {
        /**
         * The password ARN for the Windows user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-windowsuser.html#cfn-deadline-queue-windowsuser-passwordarn
         */
        readonly passwordArn: string;
        /**
         * The user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-queue-windowsuser.html#cfn-deadline-queue-windowsuser-user
         */
        readonly user: string;
    }
}
/**
 * Properties for defining a `CfnQueue`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html
 */
export interface CfnQueueProps {
    /**
     * The identifiers of the storage profiles that this queue can use to share assets between workers using different operating systems.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-allowedstorageprofileids
     */
    readonly allowedStorageProfileIds?: Array<string>;
    /**
     * The default action taken on a queue summary if a budget wasn't configured.
     *
     * @default - "NONE"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-defaultbudgetaction
     */
    readonly defaultBudgetAction?: string;
    /**
     * A description of the queue that helps identify what the queue is used for.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-description
     */
    readonly description?: string;
    /**
     * The display name of the queue summary to update.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-displayname
     */
    readonly displayName: string;
    /**
     * The farm ID.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-farmid
     */
    readonly farmId: string;
    /**
     * The job attachment settings.
     *
     * These are the Amazon S3 bucket name and the Amazon S3 prefix.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-jobattachmentsettings
     */
    readonly jobAttachmentSettings?: cdk.IResolvable | CfnQueue.JobAttachmentSettingsProperty;
    /**
     * Identifies the user for a job.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-jobrunasuser
     */
    readonly jobRunAsUser?: cdk.IResolvable | CfnQueue.JobRunAsUserProperty;
    /**
     * The file system location that the queue uses.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-requiredfilesystemlocationnames
     */
    readonly requiredFileSystemLocationNames?: Array<string>;
    /**
     * The Amazon Resource Name (ARN) of the IAM role that workers use when running jobs in this queue.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-rolearn
     */
    readonly roleArn?: string;
    /**
     * The tags to add to your queue.
     *
     * Each tag consists of a tag key and a tag value. Tag keys and values are both required, but tag values can be empty strings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queue.html#cfn-deadline-queue-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Queue resource.
 *
 * @struct
 * @stability external
 */
export interface QueueReference {
    /**
     * The Arn of the Queue resource.
     */
    readonly queueArn: string;
}
/**
 * Indicates that this resource can be referenced as a QueueEnvironment.
 *
 * @stability experimental
 */
export interface IQueueEnvironmentRef extends constructs.IConstruct {
    /**
     * A reference to a QueueEnvironment resource.
     */
    readonly queueEnvironmentRef: QueueEnvironmentReference;
}
/**
 * Creates an environment for a queue that defines how jobs in the queue run.
 *
 * @cloudformationResource AWS::Deadline::QueueEnvironment
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html
 */
export declare class CfnQueueEnvironment extends cdk.CfnResource implements cdk.IInspectable, IQueueEnvironmentRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnQueueEnvironment from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnQueueEnvironment;
    /**
     * The name of the queue environment.
     *
     * @cloudformationAttribute Name
     */
    readonly attrName: string;
    /**
     * The queue environment ID.
     *
     * @cloudformationAttribute QueueEnvironmentId
     */
    readonly attrQueueEnvironmentId: string;
    /**
     * The identifier assigned to the farm that contains the queue.
     */
    farmId: string;
    /**
     * The queue environment's priority.
     */
    priority: number;
    /**
     * The unique identifier of the queue that contains the environment.
     */
    queueId: string;
    /**
     * A JSON or YAML template that describes the processing environment for the queue.
     */
    template: string;
    /**
     * Specifies whether the template for the queue environment is JSON or YAML.
     */
    templateType: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnQueueEnvironmentProps);
    get queueEnvironmentRef(): QueueEnvironmentReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnQueueEnvironment`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html
 */
export interface CfnQueueEnvironmentProps {
    /**
     * The identifier assigned to the farm that contains the queue.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html#cfn-deadline-queueenvironment-farmid
     */
    readonly farmId: string;
    /**
     * The queue environment's priority.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html#cfn-deadline-queueenvironment-priority
     */
    readonly priority: number;
    /**
     * The unique identifier of the queue that contains the environment.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html#cfn-deadline-queueenvironment-queueid
     */
    readonly queueId: string;
    /**
     * A JSON or YAML template that describes the processing environment for the queue.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html#cfn-deadline-queueenvironment-template
     */
    readonly template: string;
    /**
     * Specifies whether the template for the queue environment is JSON or YAML.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queueenvironment.html#cfn-deadline-queueenvironment-templatetype
     */
    readonly templateType: string;
}
/**
 * A reference to a QueueEnvironment resource.
 *
 * @struct
 * @stability external
 */
export interface QueueEnvironmentReference {
    /**
     * The FarmId of the QueueEnvironment resource.
     */
    readonly farmId: string;
    /**
     * The QueueId of the QueueEnvironment resource.
     */
    readonly queueId: string;
    /**
     * The QueueEnvironmentId of the QueueEnvironment resource.
     */
    readonly queueEnvironmentId: string;
}
/**
 * Indicates that this resource can be referenced as a QueueFleetAssociation.
 *
 * @stability experimental
 */
export interface IQueueFleetAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a QueueFleetAssociation resource.
     */
    readonly queueFleetAssociationRef: QueueFleetAssociationReference;
}
/**
 * Creates an association between a queue and a fleet.
 *
 * @cloudformationResource AWS::Deadline::QueueFleetAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuefleetassociation.html
 */
export declare class CfnQueueFleetAssociation extends cdk.CfnResource implements cdk.IInspectable, IQueueFleetAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnQueueFleetAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnQueueFleetAssociation;
    /**
     * The identifier of the farm that contains the queue and the fleet.
     */
    farmId: string;
    /**
     * The fleet ID.
     */
    fleetId: string;
    /**
     * The queue ID.
     */
    queueId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnQueueFleetAssociationProps);
    get queueFleetAssociationRef(): QueueFleetAssociationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnQueueFleetAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuefleetassociation.html
 */
export interface CfnQueueFleetAssociationProps {
    /**
     * The identifier of the farm that contains the queue and the fleet.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuefleetassociation.html#cfn-deadline-queuefleetassociation-farmid
     */
    readonly farmId: string;
    /**
     * The fleet ID.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuefleetassociation.html#cfn-deadline-queuefleetassociation-fleetid
     */
    readonly fleetId: string;
    /**
     * The queue ID.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuefleetassociation.html#cfn-deadline-queuefleetassociation-queueid
     */
    readonly queueId: string;
}
/**
 * A reference to a QueueFleetAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface QueueFleetAssociationReference {
    /**
     * The FarmId of the QueueFleetAssociation resource.
     */
    readonly farmId: string;
    /**
     * The FleetId of the QueueFleetAssociation resource.
     */
    readonly fleetId: string;
    /**
     * The QueueId of the QueueFleetAssociation resource.
     */
    readonly queueId: string;
}
/**
 * Indicates that this resource can be referenced as a QueueLimitAssociation.
 *
 * @stability experimental
 */
export interface IQueueLimitAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a QueueLimitAssociation resource.
     */
    readonly queueLimitAssociationRef: QueueLimitAssociationReference;
}
/**
 * Associates a limit with a particular queue.
 *
 * After the limit is associated, all workers for jobs that specify the limit associated with the queue are subject to the limit. You can't associate two limits with the same `amountRequirementName` to the same queue.
 *
 * @cloudformationResource AWS::Deadline::QueueLimitAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuelimitassociation.html
 */
export declare class CfnQueueLimitAssociation extends cdk.CfnResource implements cdk.IInspectable, IQueueLimitAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnQueueLimitAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnQueueLimitAssociation;
    /**
     * The unique identifier of the farm that contains the queue-limit association.
     */
    farmId: string;
    /**
     * The unique identifier of the limit in the association.
     */
    limitId: string;
    /**
     * The unique identifier of the queue in the association.
     */
    queueId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnQueueLimitAssociationProps);
    get queueLimitAssociationRef(): QueueLimitAssociationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnQueueLimitAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuelimitassociation.html
 */
export interface CfnQueueLimitAssociationProps {
    /**
     * The unique identifier of the farm that contains the queue-limit association.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuelimitassociation.html#cfn-deadline-queuelimitassociation-farmid
     */
    readonly farmId: string;
    /**
     * The unique identifier of the limit in the association.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuelimitassociation.html#cfn-deadline-queuelimitassociation-limitid
     */
    readonly limitId: string;
    /**
     * The unique identifier of the queue in the association.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-queuelimitassociation.html#cfn-deadline-queuelimitassociation-queueid
     */
    readonly queueId: string;
}
/**
 * A reference to a QueueLimitAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface QueueLimitAssociationReference {
    /**
     * The FarmId of the QueueLimitAssociation resource.
     */
    readonly farmId: string;
    /**
     * The LimitId of the QueueLimitAssociation resource.
     */
    readonly limitId: string;
    /**
     * The QueueId of the QueueLimitAssociation resource.
     */
    readonly queueId: string;
}
/**
 * Indicates that this resource can be referenced as a StorageProfile.
 *
 * @stability experimental
 */
export interface IStorageProfileRef extends constructs.IConstruct {
    /**
     * A reference to a StorageProfile resource.
     */
    readonly storageProfileRef: StorageProfileReference;
}
/**
 * Creates a storage profile that specifies the operating system, file type, and file location of resources used on a farm.
 *
 * @cloudformationResource AWS::Deadline::StorageProfile
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html
 */
export declare class CfnStorageProfile extends cdk.CfnResource implements cdk.IInspectable, IStorageProfileRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnStorageProfile from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnStorageProfile;
    /**
     * The storage profile ID.
     *
     * @cloudformationAttribute StorageProfileId
     */
    readonly attrStorageProfileId: string;
    /**
     * The display name of the storage profile summary to update.
     */
    displayName: string;
    /**
     * The unique identifier of the farm that contains the storage profile.
     */
    farmId: string;
    /**
     * Operating system specific file system path to the storage location.
     */
    fileSystemLocations?: Array<CfnStorageProfile.FileSystemLocationProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * The operating system (OS) family.
     */
    osFamily: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnStorageProfileProps);
    get storageProfileRef(): StorageProfileReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnStorageProfile {
    /**
     * The details of the file system location for the resource.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-storageprofile-filesystemlocation.html
     */
    interface FileSystemLocationProperty {
        /**
         * The location name.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-storageprofile-filesystemlocation.html#cfn-deadline-storageprofile-filesystemlocation-name
         */
        readonly name: string;
        /**
         * The file path.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-storageprofile-filesystemlocation.html#cfn-deadline-storageprofile-filesystemlocation-path
         */
        readonly path: string;
        /**
         * The type of file.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-deadline-storageprofile-filesystemlocation.html#cfn-deadline-storageprofile-filesystemlocation-type
         */
        readonly type: string;
    }
}
/**
 * Properties for defining a `CfnStorageProfile`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html
 */
export interface CfnStorageProfileProps {
    /**
     * The display name of the storage profile summary to update.
     *
     * > This field can store any content. Escape or encode this content before displaying it on a webpage or any other system that might interpret the content of this field.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html#cfn-deadline-storageprofile-displayname
     */
    readonly displayName: string;
    /**
     * The unique identifier of the farm that contains the storage profile.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html#cfn-deadline-storageprofile-farmid
     */
    readonly farmId: string;
    /**
     * Operating system specific file system path to the storage location.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html#cfn-deadline-storageprofile-filesystemlocations
     */
    readonly fileSystemLocations?: Array<CfnStorageProfile.FileSystemLocationProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * The operating system (OS) family.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-deadline-storageprofile.html#cfn-deadline-storageprofile-osfamily
     */
    readonly osFamily: string;
}
/**
 * A reference to a StorageProfile resource.
 *
 * @struct
 * @stability external
 */
export interface StorageProfileReference {
    /**
     * The FarmId of the StorageProfile resource.
     */
    readonly farmId: string;
    /**
     * The StorageProfileId of the StorageProfile resource.
     */
    readonly storageProfileId: string;
}
