import { Construct } from 'constructs';
import { Billing } from './billing';
import { Capacity } from './capacity';
import { TableEncryptionV2 } from './encryption';
import { Attribute, LocalSecondaryIndexProps, SecondaryIndexProps, StreamViewType, PointInTimeRecoverySpecification, TableClass, WarmThroughput, MultiRegionConsistency, ContributorInsightsSpecification } from './shared';
import { ITableV2, TableBaseV2 } from './table-v2-base';
import { PolicyDocument } from '../../aws-iam';
import { IStream } from '../../aws-kinesis';
import { IKey } from '../../aws-kms';
import { CfnTag, RemovalPolicy, TagManager } from '../../core';
/**
 * Options used to configure global secondary indexes on a replica table.
 */
export interface ReplicaGlobalSecondaryIndexOptions extends IContributorInsightsConfigurable {
    /**
     * Whether CloudWatch contributor insights is enabled for a specific global secondary
     * index on a replica table.
     * @deprecated use `contributorInsightsSpecification` instead
     * @default - inherited from the primary table
     */
    readonly contributorInsights?: boolean;
    /**
     * Whether CloudWatch contributor insights is enabled and what mode is selected
     * for a specific global secondary index on a replica table.
     * @default - contributor insights is not enabled
     */
    readonly contributorInsightsSpecification?: ContributorInsightsSpecification;
    /**
     * The read capacity for a specific global secondary index on a replica table.
     *
     * Note: This can only be configured if primary table billing is provisioned.
     *
     * @default - inherited from the primary table
     */
    readonly readCapacity?: Capacity;
    /**
     * The maximum read request units for a specific global secondary index on a replica table.
     *
     * Note: This can only be configured if primary table billing is PAY_PER_REQUEST.
     *
     * @default - inherited from the primary table
     */
    readonly maxReadRequestUnits?: number;
}
/**
 * Properties used to configure a global secondary index.
 */
export interface GlobalSecondaryIndexPropsV2 extends SecondaryIndexProps {
    /**
     * Partition key attribute definition.
     */
    readonly partitionKey: Attribute;
    /**
     * Sort key attribute definition.
     *
     * @default - no sort key
     */
    readonly sortKey?: Attribute;
    /**
     * The read capacity.
     *
     * Note: This can only be configured if the primary table billing is provisioned.
     *
     * @default - inherited from the primary table.
     */
    readonly readCapacity?: Capacity;
    /**
     * The write capacity.
     *
     * Note: This can only be configured if the primary table billing is provisioned.
     *
     * @default - inherited from the primary table.
     */
    readonly writeCapacity?: Capacity;
    /**
     * The maximum read request units.
     *
     * Note: This can only be configured if the primary table billing is PAY_PER_REQUEST.
     *
     * @default - inherited from the primary table.
     */
    readonly maxReadRequestUnits?: number;
    /**
     * The maximum write request units.
     *
     * Note: This can only be configured if the primary table billing is PAY_PER_REQUEST.
     *
     * @default - inherited from the primary table.
     */
    readonly maxWriteRequestUnits?: number;
    /**
     * The warm throughput configuration for the global secondary index.
     *
     * @default - no warm throughput is configured
     */
    readonly warmThroughput?: WarmThroughput;
}
/**
 * Common interface for types that can configure contributor insights
 * @internal
 */
interface IContributorInsightsConfigurable {
    /**
     * Whether CloudWatch contributor insights is enabled.
     * @deprecated use `contributorInsightsSpecification` instead
     */
    readonly contributorInsights?: boolean;
    /**
     * Whether CloudWatch contributor insights is enabled and what mode is selected
     */
    readonly contributorInsightsSpecification?: ContributorInsightsSpecification;
}
/**
 * Options used to configure a DynamoDB table.
 */
export interface TableOptionsV2 extends IContributorInsightsConfigurable {
    /**
     * Whether CloudWatch contributor insights is enabled.
     * @deprecated use `contributorInsightsSpecification` instead
     * @default false
     */
    readonly contributorInsights?: boolean;
    /**
     * Whether CloudWatch contributor insights is enabled and what mode is selected
     * @default - contributor insights is not enabled
     */
    readonly contributorInsightsSpecification?: ContributorInsightsSpecification;
    /**
     * Whether deletion protection is enabled.
     *
     * @default false
     */
    readonly deletionProtection?: boolean;
    /**
     * Whether point-in-time recovery is enabled.
     * @deprecated use `pointInTimeRecoverySpecification` instead
     * @default false - point in time recovery is not enabled.
     */
    readonly pointInTimeRecovery?: boolean;
    /**
     * Whether point-in-time recovery is enabled
     * and recoveryPeriodInDays is set.
     *
     * @default - point in time recovery is not enabled.
     */
    readonly pointInTimeRecoverySpecification?: PointInTimeRecoverySpecification;
    /**
     * The table class.
     *
     * @default TableClass.STANDARD
     */
    readonly tableClass?: TableClass;
    /**
     * Kinesis Data Stream to capture item level changes.
     *
     * @default - no Kinesis Data Stream
     */
    readonly kinesisStream?: IStream;
    /**
     * Tags to be applied to the primary table (default replica table).
     *
     * @default - no tags
     */
    readonly tags?: CfnTag[];
    /**
     * Resource policy to assign to DynamoDB Table.
     * @see https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-globaltable-replicaspecification.html#cfn-dynamodb-globaltable-replicaspecification-resourcepolicy
     * @default - No resource policy statements are added to the created table.
     */
    readonly resourcePolicy?: PolicyDocument;
}
/**
 * Properties used to configure a replica table.
 */
export interface ReplicaTableProps extends TableOptionsV2 {
    /**
     * The region that the replica table will be created in.
     */
    readonly region: string;
    /**
     * The read capacity.
     *
     * Note: This can only be configured if the primary table billing is provisioned.
     *
     * @default - inherited from the primary table
     */
    readonly readCapacity?: Capacity;
    /**
     * The maximum read request units.
     *
     * Note: This can only be configured if the primary table billing is PAY_PER_REQUEST.
     *
     * @default - inherited from the primary table
     */
    readonly maxReadRequestUnits?: number;
    /**
     * Options used to configure global secondary index properties.
     *
     * @default - inherited from the primary table
     */
    readonly globalSecondaryIndexOptions?: {
        [indexName: string]: ReplicaGlobalSecondaryIndexOptions;
    };
}
/**
 * Properties used to configure a DynamoDB table.
 */
export interface TablePropsV2 extends TableOptionsV2 {
    /**
     * Partition key attribute definition.
     */
    readonly partitionKey: Attribute;
    /**
     * Sort key attribute definition.
     *
     * @default - no sort key
     */
    readonly sortKey?: Attribute;
    /**
     * The name of the table.
     *
     * @default - generated by CloudFormation
     */
    readonly tableName?: string;
    /**
     * The name of the TTL attribute.
     *
     * @default - TTL is disabled
     */
    readonly timeToLiveAttribute?: string;
    /**
     * When an item in the table is modified, StreamViewType determines what information is
     * written to the stream.
     *
     * @default - streams are disabled if replicas are not configured and this property is
     * not specified. If this property is not specified when replicas are configured, then
     * NEW_AND_OLD_IMAGES will be the StreamViewType for all replicas
     */
    readonly dynamoStream?: StreamViewType;
    /**
     * The removal policy applied to the table.
     *
     * @default RemovalPolicy.RETAIN
     */
    readonly removalPolicy?: RemovalPolicy;
    /**
     * The billing mode and capacity settings to apply to the table.
     *
     * @default Billing.onDemand()
     */
    readonly billing?: Billing;
    /**
     * Replica tables to deploy with the primary table.
     *
     * Note: Adding replica tables allows you to use your table as a global table. You
     * cannot specify a replica table in the region that the primary table will be deployed
     * to. Replica tables will only be supported if the stack deployment region is defined.
     *
     * @default - no replica tables
     */
    readonly replicas?: ReplicaTableProps[];
    /**
     * The witness Region for the MRSC global table.
     * A MRSC global table can be configured with either three replicas, or with two replicas and one witness.
     *
     * Note: Witness region cannot be specified for a Multi-Region Eventual Consistency (MREC) Global Table.
     * Witness regions are only supported for Multi-Region Strong Consistency (MRSC) Global Tables.
     *
     * @default - no witness region
     */
    readonly witnessRegion?: string;
    /**
     * Specifies the consistency mode for a new global table.
     *
     * @default MultiRegionConsistency.EVENTUAL
     */
    readonly multiRegionConsistency?: MultiRegionConsistency;
    /**
     * Global secondary indexes.
     *
     * Note: You can provide a maximum of 20 global secondary indexes.
     *
     * @default - no global secondary indexes
     */
    readonly globalSecondaryIndexes?: GlobalSecondaryIndexPropsV2[];
    /**
     * Local secondary indexes.
     *
     * Note: You can only provide a maximum of 5 local secondary indexes.
     *
     * @default - no local secondary indexes
     */
    readonly localSecondaryIndexes?: LocalSecondaryIndexProps[];
    /**
     * The server-side encryption.
     *
     * @default TableEncryptionV2.dynamoOwnedKey()
     */
    readonly encryption?: TableEncryptionV2;
    /**
     * The warm throughput configuration for the table.
     *
     * @default - no warm throughput is configured
     */
    readonly warmThroughput?: WarmThroughput;
}
/**
 * Attributes of a DynamoDB table.
 */
export interface TableAttributesV2 {
    /**
     * The ARN of the table.
     *
     * Note: You must specify this or the `tableName`.
     *
     * @default - table arn generated using `tableName` and region of stack
     */
    readonly tableArn?: string;
    /**
     * The name of the table.
     *
     * Note: You must specify this or the `tableArn`.
     *
     * @default - table name retrieved from provided `tableArn`
     */
    readonly tableName?: string;
    /**
     * The ID of the table.
     *
     * @default - no table id
     */
    readonly tableId?: string;
    /**
     * The stream ARN of the table.
     *
     * @default - no table stream ARN
     */
    readonly tableStreamArn?: string;
    /**
     * KMS encryption key for the table.
     *
     * @default - no KMS encryption key
     */
    readonly encryptionKey?: IKey;
    /**
     * The name of the global indexes set for the table.
     *
     * Note: You must set either this property or `localIndexes` if you want permissions
     * to be granted for indexes as well as the table itself.
     *
     * @default - no global indexes
     */
    readonly globalIndexes?: string[];
    /**
     * The name of the local indexes set for the table.
     *
     * Note: You must set either this property or `globalIndexes` if you want permissions
     * to be granted for indexes as well as the table itself.
     *
     * @default - no local indexes
     */
    readonly localIndexes?: string[];
    /**
     * Whether or not to grant permissions for all indexes of the table.
     *
     * Note: If false, permissions will only be granted to indexes when `globalIndexes`
     * or `localIndexes` is specified.
     *
     * @default false
     */
    readonly grantIndexPermissions?: boolean;
}
/**
 * A DynamoDB Table.
 */
export declare class TableV2 extends TableBaseV2 {
    /**
     * Uniquely identifies this class.
     */
    static readonly PROPERTY_INJECTION_ID: string;
    /**
     * Creates a Table construct that represents an external table via table name.
     *
     * @param scope the parent creating construct (usually `this`)
     * @param id the construct's name
     * @param tableName the table's name
     */
    static fromTableName(scope: Construct, id: string, tableName: string): ITableV2;
    /**
     * Creates a Table construct that represents an external table via table ARN.
     *
     * @param scope the parent creating construct (usually `this`)
     * @param id the construct's name
     * @param tableArn the table's ARN
     */
    static fromTableArn(scope: Construct, id: string, tableArn: string): ITableV2;
    /**
     * Creates a Table construct that represents an external table.
     *
     * @param scope the parent creating construct (usually `this`)
     * @param id the construct's name
     * @param attrs attributes of the table
     */
    static fromTableAttributes(scope: Construct, id: string, attrs: TableAttributesV2): ITableV2;
    /**
     * @attribute
     */
    readonly tableArn: string;
    /**
     * @attribute
     */
    readonly tableName: string;
    /**
     * @attribute
     */
    readonly tableStreamArn?: string;
    /**
     * @attribute
     */
    readonly tableId?: string;
    readonly encryptionKey?: IKey;
    /**
     * @attribute
     */
    resourcePolicy?: PolicyDocument;
    protected readonly region: string;
    protected readonly tags: TagManager;
    private readonly billingMode;
    private readonly partitionKey;
    private readonly hasSortKey;
    private readonly tableOptions;
    private readonly encryption?;
    private readonly keySchema;
    private readonly attributeDefinitions;
    private readonly nonKeyAttributes;
    private readonly readProvisioning?;
    private readonly writeProvisioning?;
    private readonly maxReadRequestUnits?;
    private readonly maxWriteRequestUnits?;
    private readonly replicaTables;
    private readonly replicaKeys;
    private readonly replicaTableArns;
    private readonly replicaStreamArns;
    private readonly globalSecondaryIndexes;
    private readonly localSecondaryIndexes;
    private readonly globalSecondaryIndexReadCapacitys;
    private readonly globalSecondaryIndexMaxReadUnits;
    constructor(scope: Construct, id: string, props: TablePropsV2);
    /**
     * Add a replica table.
     *
     * Note: Adding a replica table will allow you to use your table as a global table.
     *
     * @param props the properties of the replica table to add
     */
    addReplica(props: ReplicaTableProps): void;
    /**
     * Add a global secondary index to the table.
     *
     * Note: Global secondary indexes will be inherited by all replica tables.
     *
     * @param props the properties of the global secondary index
     */
    addGlobalSecondaryIndex(props: GlobalSecondaryIndexPropsV2): void;
    /**
     * Add a local secondary index to the table.
     *
     * Note: Local secondary indexes will be inherited by all replica tables.
     *
     * @param props the properties of the local secondary index
     */
    addLocalSecondaryIndex(props: LocalSecondaryIndexProps): void;
    /**
     * Retrieve a replica table.
     *
     * Note: Replica tables are not supported in a region agnostic stack.
     *
     * @param region the region of the replica table
     */
    replica(region: string): ITableV2;
    private configureReplicaTable;
    private configureGlobalSecondaryIndex;
    private configureLocalSecondaryIndex;
    private configureReplicaGlobalSecondaryIndexes;
    private configureIndexKeySchema;
    private configureIndexProjection;
    private configureReplicaKeys;
    private renderReplicaTables;
    private renderGlobalIndexes;
    private renderLocalIndexes;
    private renderStreamSpecification;
    private addKey;
    private addAttributeDefinition;
    protected get hasIndex(): boolean;
    private validateIndexName;
    private validateIndexProjection;
    private validateReplicaIndexOptions;
    private validateReplica;
    private validateGlobalSecondaryIndex;
    private validateLocalSecondaryIndex;
    private validatePitr;
    private validateMrscConfiguration;
    private validateCCI;
}
export {};
