import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a EmailContact.
 *
 * @stability experimental
 */
export interface IEmailContactRef extends constructs.IConstruct {
    /**
     * A reference to a EmailContact resource.
     */
    readonly emailContactRef: EmailContactReference;
}
/**
 * Configures email contacts for AWS User Notifications .
 *
 * @cloudformationResource AWS::NotificationsContacts::EmailContact
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notificationscontacts-emailcontact.html
 */
export declare class CfnEmailContact extends cdk.CfnResource implements cdk.IInspectable, IEmailContactRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnEmailContact from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnEmailContact;
    /**
     * Returns the ARN of the contact.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * @cloudformationAttribute EmailContact
     */
    readonly attrEmailContact: cdk.IResolvable;
    /**
     * The email address of the contact.
     *
     * @cloudformationAttribute EmailContact.Address
     */
    readonly attrEmailContactAddress: string;
    /**
     * The Amazon Resource Name (ARN) of the contact.
     *
     * @cloudformationAttribute EmailContact.Arn
     */
    readonly attrEmailContactArn: string;
    /**
     * The creation time of the `EmailContact` .
     *
     * @cloudformationAttribute EmailContact.CreationTime
     */
    readonly attrEmailContactCreationTime: string;
    /**
     * The name of the contact.
     *
     * @cloudformationAttribute EmailContact.Name
     */
    readonly attrEmailContactName: string;
    /**
     * The status of the contact. Only activated contacts receive emails.
     *
     * @cloudformationAttribute EmailContact.Status
     */
    readonly attrEmailContactStatus: string;
    /**
     * The time the `EmailContact` was last updated.
     *
     * @cloudformationAttribute EmailContact.UpdateTime
     */
    readonly attrEmailContactUpdateTime: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The email address of the contact.
     */
    emailAddress: string;
    /**
     * The name of the contact.
     */
    name: string;
    /**
     * A list of tags to apply to the email contact.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnEmailContactProps);
    get emailContactRef(): EmailContactReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnEmailContact {
    /**
     * Configures email contacts for AWS User Notifications .
     *
     * You must activate the email contact using the activation token that you will receive when the email contact is set up.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html
     */
    interface EmailContactProperty {
        /**
         * The email address of the contact.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-address
         */
        readonly address: string;
        /**
         * The Amazon Resource Name (ARN) of the contact.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-arn
         */
        readonly arn: string;
        /**
         * The creation time of the `EmailContact` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-creationtime
         */
        readonly creationTime: string;
        /**
         * The name of the contact.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-name
         */
        readonly name: string;
        /**
         * The status of the contact.
         *
         * Only activated contacts receive emails.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-status
         */
        readonly status: string;
        /**
         * The time the `EmailContact` was last updated.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notificationscontacts-emailcontact-emailcontact.html#cfn-notificationscontacts-emailcontact-emailcontact-updatetime
         */
        readonly updateTime: string;
    }
}
/**
 * Properties for defining a `CfnEmailContact`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notificationscontacts-emailcontact.html
 */
export interface CfnEmailContactProps {
    /**
     * The email address of the contact.
     *
     * The activation and notification emails are sent here.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notificationscontacts-emailcontact.html#cfn-notificationscontacts-emailcontact-emailaddress
     */
    readonly emailAddress: string;
    /**
     * The name of the contact.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notificationscontacts-emailcontact.html#cfn-notificationscontacts-emailcontact-name
     */
    readonly name: string;
    /**
     * A list of tags to apply to the email contact.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notificationscontacts-emailcontact.html#cfn-notificationscontacts-emailcontact-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a EmailContact resource.
 *
 * @struct
 * @stability external
 */
export interface EmailContactReference {
    /**
     * The Arn of the EmailContact resource.
     */
    readonly emailContactArn: string;
}
