import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a Namespace.
 *
 * @stability experimental
 */
export interface INamespaceRef extends constructs.IConstruct {
    /**
     * A reference to a Namespace resource.
     */
    readonly namespaceRef: NamespaceReference;
}
/**
 * Creates a namespace.
 *
 * A namespace is a logical grouping of tables within your table bucket, which you can use to organize tables. For more information, see [Create a namespace](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-namespace-create.html) in the *Amazon Simple Storage Service User Guide* .
 *
 * - **Permissions** - You must have the `s3tables:CreateNamespace` permission to use this operation.
 * - **Cloud Development Kit** - To use S3 Tables AWS CDK constructs, add the `@aws-cdk/aws-s3tables-alpha` dependency with one of the following options:
 *
 * - NPM: `npm i @aws-cdk/aws-s3tables-alpha`
 * - Yarn: `yarn add @aws-cdk/aws-s3tables-alpha`
 *
 * @cloudformationResource AWS::S3Tables::Namespace
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-namespace.html
 */
export declare class CfnNamespace extends cdk.CfnResource implements cdk.IInspectable, INamespaceRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnNamespace from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnNamespace;
    /**
     * The name of the namespace.
     */
    namespace: string;
    /**
     * The Amazon Resource Name (ARN) of the table bucket to create the namespace in.
     */
    tableBucketArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnNamespaceProps);
    get namespaceRef(): NamespaceReference;
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
 * Properties for defining a `CfnNamespace`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-namespace.html
 */
export interface CfnNamespaceProps {
    /**
     * The name of the namespace.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-namespace.html#cfn-s3tables-namespace-namespace
     */
    readonly namespace: string;
    /**
     * The Amazon Resource Name (ARN) of the table bucket to create the namespace in.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-namespace.html#cfn-s3tables-namespace-tablebucketarn
     */
    readonly tableBucketArn: string;
}
/**
 * A reference to a Namespace resource.
 *
 * @struct
 * @stability external
 */
export interface NamespaceReference {
    /**
     * The TableBucketARN of the Namespace resource.
     */
    readonly tableBucketArn: string;
    /**
     * The Namespace of the Namespace resource.
     */
    readonly namespace: string;
}
/**
 * Indicates that this resource can be referenced as a Table.
 *
 * @stability experimental
 */
export interface ITableRef extends constructs.IConstruct {
    /**
     * A reference to a Table resource.
     */
    readonly tableRef: TableReference;
}
/**
 * Creates a new table associated with the given namespace in a table bucket.
 *
 * For more information, see [Creating an Amazon S3 table](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-create.html) in the *Amazon Simple Storage Service User Guide* .
 *
 * - **Permissions** - - You must have the `s3tables:CreateTable` permission to use this operation.
 * - If you use this operation with the optional `metadata` request parameter you must have the `s3tables:PutTableData` permission.
 * - If you use this operation with the optional `encryptionConfiguration` request parameter you must have the `s3tables:PutTableEncryption` permission.
 *
 * > Additionally, If you choose SSE-KMS encryption you must grant the S3 Tables maintenance principal access to your KMS key. For more information, see [Permissions requirements for S3 Tables SSE-KMS encryption](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-kms-permissions.html) .
 * - **Cloud Development Kit** - To use S3 Tables AWS CDK constructs, add the `@aws-cdk/aws-s3tables-alpha` dependency with one of the following options:
 *
 * - NPM: `npm i @aws-cdk/aws-s3tables-alpha`
 * - Yarn: `yarn add @aws-cdk/aws-s3tables-alpha`
 *
 * @cloudformationResource AWS::S3Tables::Table
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html
 */
export declare class CfnTable extends cdk.CfnResource implements cdk.IInspectable, ITableRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTable from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTable;
    /**
     * The Amazon Resource Name (ARN) of the table.
     *
     * @cloudformationAttribute TableARN
     */
    readonly attrTableArn: string;
    /**
     * The version token of the table.
     *
     * @cloudformationAttribute VersionToken
     */
    readonly attrVersionToken: string;
    /**
     * The warehouse location of the table.
     *
     * @cloudformationAttribute WarehouseLocation
     */
    readonly attrWarehouseLocation: string;
    /**
     * Contains details about the compaction settings for an Iceberg table.
     */
    compaction?: CfnTable.CompactionProperty | cdk.IResolvable;
    /**
     * Contains details about the metadata for an Iceberg table.
     */
    icebergMetadata?: CfnTable.IcebergMetadataProperty | cdk.IResolvable;
    /**
     * The name of the namespace.
     */
    namespace: string;
    /**
     * The format of the table.
     */
    openTableFormat: string;
    /**
     * Contains details about the Iceberg snapshot management settings for the table.
     */
    snapshotManagement?: cdk.IResolvable | CfnTable.SnapshotManagementProperty;
    /**
     * The Amazon Resource Name (ARN) of the table bucket to create the table in.
     */
    tableBucketArn: string;
    /**
     * The name for the table.
     */
    tableName: string;
    /**
     * Indicates that you don't want to specify a schema for the table.
     */
    withoutMetadata?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTableProps);
    get tableRef(): TableReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnTable {
    /**
     * Contains details about the compaction settings for an Iceberg table.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-compaction.html
     */
    interface CompactionProperty {
        /**
         * The status of the maintenance configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-compaction.html#cfn-s3tables-table-compaction-status
         */
        readonly status?: string;
        /**
         * The target file size for the table in MB.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-compaction.html#cfn-s3tables-table-compaction-targetfilesizemb
         */
        readonly targetFileSizeMb?: number;
    }
    /**
     * Contains details about the metadata for an Iceberg table.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-icebergmetadata.html
     */
    interface IcebergMetadataProperty {
        /**
         * The schema for an Iceberg table.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-icebergmetadata.html#cfn-s3tables-table-icebergmetadata-icebergschema
         */
        readonly icebergSchema: CfnTable.IcebergSchemaProperty | cdk.IResolvable;
    }
    /**
     * Contains details about the schema for an Iceberg table.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-icebergschema.html
     */
    interface IcebergSchemaProperty {
        /**
         * The schema fields for the table.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-icebergschema.html#cfn-s3tables-table-icebergschema-schemafieldlist
         */
        readonly schemaFieldList: Array<cdk.IResolvable | CfnTable.SchemaFieldProperty> | cdk.IResolvable;
    }
    /**
     * Contains details about a schema field.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-schemafield.html
     */
    interface SchemaFieldProperty {
        /**
         * The name of the field.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-schemafield.html#cfn-s3tables-table-schemafield-name
         */
        readonly name: string;
        /**
         * A Boolean value that specifies whether values are required for each row in this field.
         *
         * By default, this is `false` and null values are allowed in the field. If this is `true` the field does not allow null values.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-schemafield.html#cfn-s3tables-table-schemafield-required
         */
        readonly required?: boolean | cdk.IResolvable;
        /**
         * The field type.
         *
         * S3 Tables supports all Apache Iceberg primitive types. For more information, see the [Apache Iceberg documentation](https://docs.aws.amazon.com/https://iceberg.apache.org/spec/#primitive-types) .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-schemafield.html#cfn-s3tables-table-schemafield-type
         */
        readonly type: string;
    }
    /**
     * Contains details about the snapshot management settings for an Iceberg table.
     *
     * The oldest snapshot expires when its age exceeds the `maxSnapshotAgeHours` and the total number of snapshots exceeds the value for the minimum number of snapshots to keep `minSnapshotsToKeep` .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-snapshotmanagement.html
     */
    interface SnapshotManagementProperty {
        /**
         * The maximum age of a snapshot before it can be expired.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-snapshotmanagement.html#cfn-s3tables-table-snapshotmanagement-maxsnapshotagehours
         */
        readonly maxSnapshotAgeHours?: number;
        /**
         * The minimum number of snapshots to keep.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-snapshotmanagement.html#cfn-s3tables-table-snapshotmanagement-minsnapshotstokeep
         */
        readonly minSnapshotsToKeep?: number;
        /**
         * The status of the maintenance configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-table-snapshotmanagement.html#cfn-s3tables-table-snapshotmanagement-status
         */
        readonly status?: string;
    }
}
/**
 * Properties for defining a `CfnTable`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html
 */
export interface CfnTableProps {
    /**
     * Contains details about the compaction settings for an Iceberg table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-compaction
     */
    readonly compaction?: CfnTable.CompactionProperty | cdk.IResolvable;
    /**
     * Contains details about the metadata for an Iceberg table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-icebergmetadata
     */
    readonly icebergMetadata?: CfnTable.IcebergMetadataProperty | cdk.IResolvable;
    /**
     * The name of the namespace.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-namespace
     */
    readonly namespace: string;
    /**
     * The format of the table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-opentableformat
     */
    readonly openTableFormat: string;
    /**
     * Contains details about the Iceberg snapshot management settings for the table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-snapshotmanagement
     */
    readonly snapshotManagement?: cdk.IResolvable | CfnTable.SnapshotManagementProperty;
    /**
     * The Amazon Resource Name (ARN) of the table bucket to create the table in.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-tablebucketarn
     */
    readonly tableBucketArn: string;
    /**
     * The name for the table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-tablename
     */
    readonly tableName: string;
    /**
     * Indicates that you don't want to specify a schema for the table.
     *
     * This property is mutually exclusive to `IcebergMetadata` , and its only possible value is `Yes` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-table.html#cfn-s3tables-table-withoutmetadata
     */
    readonly withoutMetadata?: string;
}
/**
 * A reference to a Table resource.
 *
 * @struct
 * @stability external
 */
export interface TableReference {
    /**
     * The TableARN of the Table resource.
     */
    readonly tableArn: string;
}
/**
 * Indicates that this resource can be referenced as a TableBucket.
 *
 * @stability experimental
 */
export interface ITableBucketRef extends constructs.IConstruct {
    /**
     * A reference to a TableBucket resource.
     */
    readonly tableBucketRef: TableBucketReference;
}
/**
 * Creates a table bucket.
 *
 * For more information, see [Creating a table bucket](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-buckets-create.html) in the *Amazon Simple Storage Service User Guide* .
 *
 * - **Permissions** - - You must have the `s3tables:CreateTableBucket` permission to use this operation.
 * - If you use this operation with the optional `encryptionConfiguration` parameter you must have the `s3tables:PutTableBucketEncryption` permission.
 * - **Cloud Development Kit** - To use S3 Tables AWS CDK constructs, add the `@aws-cdk/aws-s3tables-alpha` dependency with one of the following options:
 *
 * - NPM: `npm i @aws-cdk/aws-s3tables-alpha`
 * - Yarn: `yarn add @aws-cdk/aws-s3tables-alpha`
 *
 * @cloudformationResource AWS::S3Tables::TableBucket
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucket.html
 */
export declare class CfnTableBucket extends cdk.CfnResource implements cdk.IInspectable, ITableBucketRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTableBucket from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTableBucket;
    /**
     * The Amazon Resource Name (ARN) of the table bucket.
     *
     * @cloudformationAttribute TableBucketARN
     */
    readonly attrTableBucketArn: string;
    /**
     * Configuration specifying how data should be encrypted.
     */
    encryptionConfiguration?: CfnTableBucket.EncryptionConfigurationProperty | cdk.IResolvable;
    /**
     * The name for the table bucket.
     */
    tableBucketName: string;
    /**
     * The unreferenced file removal settings for your table bucket.
     */
    unreferencedFileRemoval?: cdk.IResolvable | CfnTableBucket.UnreferencedFileRemovalProperty;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTableBucketProps);
    get tableBucketRef(): TableBucketReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnTableBucket {
    /**
     * The unreferenced file removal settings for your table bucket.
     *
     * Unreferenced file removal identifies and deletes all objects that are not referenced by any table snapshots. For more information, see the [*Amazon S3 User Guide*](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-table-buckets-maintenance.html) .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-unreferencedfileremoval.html
     */
    interface UnreferencedFileRemovalProperty {
        /**
         * The number of days an object can be noncurrent before Amazon S3 deletes it.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-unreferencedfileremoval.html#cfn-s3tables-tablebucket-unreferencedfileremoval-noncurrentdays
         */
        readonly noncurrentDays?: number;
        /**
         * The status of the unreferenced file removal configuration for your table bucket.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-unreferencedfileremoval.html#cfn-s3tables-tablebucket-unreferencedfileremoval-status
         */
        readonly status?: string;
        /**
         * The number of days an object must be unreferenced by your table before Amazon S3 marks the object as noncurrent.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-unreferencedfileremoval.html#cfn-s3tables-tablebucket-unreferencedfileremoval-unreferenceddays
         */
        readonly unreferencedDays?: number;
    }
    /**
     * Configuration specifying how data should be encrypted.
     *
     * This structure defines the encryption algorithm and optional KMS key to be used for server-side encryption.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-encryptionconfiguration.html
     */
    interface EncryptionConfigurationProperty {
        /**
         * The Amazon Resource Name (ARN) of the KMS key to use for encryption.
         *
         * This field is required only when `sseAlgorithm` is set to `aws:kms` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-encryptionconfiguration.html#cfn-s3tables-tablebucket-encryptionconfiguration-kmskeyarn
         */
        readonly kmsKeyArn?: string;
        /**
         * The server-side encryption algorithm to use.
         *
         * Valid values are `AES256` for S3-managed encryption keys, or `aws:kms` for AWS KMS-managed encryption keys. If you choose SSE-KMS encryption you must grant the S3 Tables maintenance principal access to your KMS key. For more information, see [Permissions requirements for S3 Tables SSE-KMS encryption](https://docs.aws.amazon.com//AmazonS3/latest/userguide/s3-tables-kms-permissions.html) .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3tables-tablebucket-encryptionconfiguration.html#cfn-s3tables-tablebucket-encryptionconfiguration-ssealgorithm
         */
        readonly sseAlgorithm?: string;
    }
}
/**
 * Properties for defining a `CfnTableBucket`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucket.html
 */
export interface CfnTableBucketProps {
    /**
     * Configuration specifying how data should be encrypted.
     *
     * This structure defines the encryption algorithm and optional KMS key to be used for server-side encryption.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucket.html#cfn-s3tables-tablebucket-encryptionconfiguration
     */
    readonly encryptionConfiguration?: CfnTableBucket.EncryptionConfigurationProperty | cdk.IResolvable;
    /**
     * The name for the table bucket.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucket.html#cfn-s3tables-tablebucket-tablebucketname
     */
    readonly tableBucketName: string;
    /**
     * The unreferenced file removal settings for your table bucket.
     *
     * Unreferenced file removal identifies and deletes all objects that are not referenced by any table snapshots. For more information, see the [*Amazon S3 User Guide*](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-table-buckets-maintenance.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucket.html#cfn-s3tables-tablebucket-unreferencedfileremoval
     */
    readonly unreferencedFileRemoval?: cdk.IResolvable | CfnTableBucket.UnreferencedFileRemovalProperty;
}
/**
 * A reference to a TableBucket resource.
 *
 * @struct
 * @stability external
 */
export interface TableBucketReference {
    /**
     * The TableBucketARN of the TableBucket resource.
     */
    readonly tableBucketArn: string;
}
/**
 * Indicates that this resource can be referenced as a TableBucketPolicy.
 *
 * @stability experimental
 */
export interface ITableBucketPolicyRef extends constructs.IConstruct {
    /**
     * A reference to a TableBucketPolicy resource.
     */
    readonly tableBucketPolicyRef: TableBucketPolicyReference;
}
/**
 * Creates a new maintenance configuration or replaces an existing table bucket policy for a table bucket.
 *
 * For more information, see [Adding a table bucket policy](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-bucket-policy.html#table-bucket-policy-add) in the *Amazon Simple Storage Service User Guide* .
 *
 * - **Permissions** - You must have the `s3tables:PutTableBucketPolicy` permission to use this operation.
 * - **Cloud Development Kit** - To use S3 Tables AWS CDK constructs, add the `@aws-cdk/aws-s3tables-alpha` dependency with one of the following options:
 *
 * - NPM: `npm i @aws-cdk/aws-s3tables-alpha`
 * - Yarn: `yarn add @aws-cdk/aws-s3tables-alpha`
 *
 * @cloudformationResource AWS::S3Tables::TableBucketPolicy
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucketpolicy.html
 */
export declare class CfnTableBucketPolicy extends cdk.CfnResource implements cdk.IInspectable, ITableBucketPolicyRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTableBucketPolicy from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTableBucketPolicy;
    /**
     * The bucket policy JSON for the table bucket.
     */
    resourcePolicy: any | cdk.IResolvable | string;
    /**
     * The Amazon Resource Name (ARN) of the table bucket.
     */
    tableBucketArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTableBucketPolicyProps);
    get tableBucketPolicyRef(): TableBucketPolicyReference;
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
 * Properties for defining a `CfnTableBucketPolicy`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucketpolicy.html
 */
export interface CfnTableBucketPolicyProps {
    /**
     * The bucket policy JSON for the table bucket.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucketpolicy.html#cfn-s3tables-tablebucketpolicy-resourcepolicy
     */
    readonly resourcePolicy: any | cdk.IResolvable | string;
    /**
     * The Amazon Resource Name (ARN) of the table bucket.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablebucketpolicy.html#cfn-s3tables-tablebucketpolicy-tablebucketarn
     */
    readonly tableBucketArn: string;
}
/**
 * A reference to a TableBucketPolicy resource.
 *
 * @struct
 * @stability external
 */
export interface TableBucketPolicyReference {
    /**
     * The TableBucketARN of the TableBucketPolicy resource.
     */
    readonly tableBucketArn: string;
}
/**
 * Indicates that this resource can be referenced as a TablePolicy.
 *
 * @stability experimental
 */
export interface ITablePolicyRef extends constructs.IConstruct {
    /**
     * A reference to a TablePolicy resource.
     */
    readonly tablePolicyRef: TablePolicyReference;
}
/**
 * Creates a new maintenance configuration or replaces an existing table policy for a table.
 *
 * For more information, see [Adding a table policy](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables-table-policy.html#table-policy-add) in the *Amazon Simple Storage Service User Guide* .
 *
 * - **Permissions** - You must have the `s3tables:PutTablePolicy` permission to use this operation.
 * - **Cloud Development Kit** - To use S3 Tables AWS CDK constructs, add the `@aws-cdk/aws-s3tables-alpha` dependency with one of the following options:
 *
 * - NPM: `npm i @aws-cdk/aws-s3tables-alpha`
 * - Yarn: `yarn add @aws-cdk/aws-s3tables-alpha`
 *
 * @cloudformationResource AWS::S3Tables::TablePolicy
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablepolicy.html
 */
export declare class CfnTablePolicy extends cdk.CfnResource implements cdk.IInspectable, ITablePolicyRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTablePolicy from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTablePolicy;
    /**
     * The namespace to associated with the table.
     *
     * @cloudformationAttribute Namespace
     */
    readonly attrNamespace: string;
    /**
     * The Amazon Resource Name (ARN) of the table bucket that contains the table.
     *
     * @cloudformationAttribute TableBucketARN
     */
    readonly attrTableBucketArn: string;
    /**
     * The name of the table.
     *
     * @cloudformationAttribute TableName
     */
    readonly attrTableName: string;
    /**
     * The `JSON` that defines the policy.
     */
    resourcePolicy: any | cdk.IResolvable | string;
    /**
     * The Amazon Resource Name (ARN) of the table.
     */
    tableArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTablePolicyProps);
    get tablePolicyRef(): TablePolicyReference;
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
 * Properties for defining a `CfnTablePolicy`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablepolicy.html
 */
export interface CfnTablePolicyProps {
    /**
     * The `JSON` that defines the policy.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablepolicy.html#cfn-s3tables-tablepolicy-resourcepolicy
     */
    readonly resourcePolicy: any | cdk.IResolvable | string;
    /**
     * The Amazon Resource Name (ARN) of the table.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-s3tables-tablepolicy.html#cfn-s3tables-tablepolicy-tablearn
     */
    readonly tableArn: string;
}
/**
 * A reference to a TablePolicy resource.
 *
 * @struct
 * @stability external
 */
export interface TablePolicyReference {
    /**
     * The TableARN of the TablePolicy resource.
     */
    readonly tableArn: string;
}
