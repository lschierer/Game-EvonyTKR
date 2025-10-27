import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a Preferences.
 *
 * @stability experimental
 */
export interface IPreferencesRef extends constructs.IConstruct {
    /**
     * A reference to a Preferences resource.
     */
    readonly preferencesRef: PreferencesReference;
}
/**
 * Specify new or changed connection recording preferences for your AWS Systems Manager GUI Connect connections.
 *
 * @cloudformationResource AWS::SSMGuiConnect::Preferences
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ssmguiconnect-preferences.html
 */
export declare class CfnPreferences extends cdk.CfnResource implements cdk.IInspectable, IPreferencesRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnPreferences from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnPreferences;
    /**
     * The primary identifier for the AWS CloudFormation resource.
     *
     * @cloudformationAttribute AccountId
     */
    readonly attrAccountId: string;
    /**
     * The set of preferences used for recording RDP connections in the requesting AWS account and AWS Region .
     */
    connectionRecordingPreferences?: CfnPreferences.ConnectionRecordingPreferencesProperty | cdk.IResolvable;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnPreferencesProps);
    get preferencesRef(): PreferencesReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnPreferences {
    /**
     * The set of preferences used for recording RDP connections in the requesting AWS account and AWS Region .
     *
     * This includes details such as which S3 bucket recordings are stored in.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-connectionrecordingpreferences.html
     */
    interface ConnectionRecordingPreferencesProperty {
        /**
         * The ARN of a AWS KMS key that is used to encrypt data while it is being processed by the service.
         *
         * This key must exist in the same AWS Region as the node you start an RDP connection to.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-connectionrecordingpreferences.html#cfn-ssmguiconnect-preferences-connectionrecordingpreferences-kmskeyarn
         */
        readonly kmsKeyArn: string;
        /**
         * Determines where recordings of RDP connections are stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-connectionrecordingpreferences.html#cfn-ssmguiconnect-preferences-connectionrecordingpreferences-recordingdestinations
         */
        readonly recordingDestinations: cdk.IResolvable | CfnPreferences.RecordingDestinationsProperty;
    }
    /**
     * Determines where recordings of RDP connections are stored.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-recordingdestinations.html
     */
    interface RecordingDestinationsProperty {
        /**
         * The S3 bucket where RDP connection recordings are stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-recordingdestinations.html#cfn-ssmguiconnect-preferences-recordingdestinations-s3buckets
         */
        readonly s3Buckets: Array<cdk.IResolvable | CfnPreferences.S3BucketProperty> | cdk.IResolvable;
    }
    /**
     * The S3 bucket where RDP connection recordings are stored.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-s3bucket.html
     */
    interface S3BucketProperty {
        /**
         * The name of the S3 bucket where RDP connection recordings are stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-s3bucket.html#cfn-ssmguiconnect-preferences-s3bucket-bucketname
         */
        readonly bucketName: string;
        /**
         * The AWS account number that owns the S3 bucket.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssmguiconnect-preferences-s3bucket.html#cfn-ssmguiconnect-preferences-s3bucket-bucketowner
         */
        readonly bucketOwner: string;
    }
}
/**
 * Properties for defining a `CfnPreferences`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ssmguiconnect-preferences.html
 */
export interface CfnPreferencesProps {
    /**
     * The set of preferences used for recording RDP connections in the requesting AWS account and AWS Region .
     *
     * This includes details such as which S3 bucket recordings are stored in.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ssmguiconnect-preferences.html#cfn-ssmguiconnect-preferences-connectionrecordingpreferences
     */
    readonly connectionRecordingPreferences?: CfnPreferences.ConnectionRecordingPreferencesProperty | cdk.IResolvable;
}
/**
 * A reference to a Preferences resource.
 *
 * @struct
 * @stability external
 */
export interface PreferencesReference {
    /**
     * The AccountId of the Preferences resource.
     */
    readonly accountId: string;
}
