import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a ChannelAssociation.
 *
 * @stability experimental
 */
export interface IChannelAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ChannelAssociation resource.
     */
    readonly channelAssociationRef: ChannelAssociationReference;
}
/**
 * The `AWS::Notifications::ChannelAssociation` resource associates a `Channel` with a `NotificationConfiguration` for AWS User Notifications .
 *
 * For more information about AWS User Notifications , see the [AWS User Notifications User Guide](https://docs.aws.amazon.com/notifications/latest/userguide/what-is-service.html) .
 *
 * @cloudformationResource AWS::Notifications::ChannelAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-channelassociation.html
 */
export declare class CfnChannelAssociation extends cdk.CfnResource implements cdk.IInspectable, IChannelAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnChannelAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnChannelAssociation;
    /**
     * The Amazon Resource Name (ARN) of the `Channel` .
     */
    arn: string;
    /**
     * The ARN of the `NotificationConfiguration` associated with the `Channel` .
     */
    notificationConfigurationArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnChannelAssociationProps);
    get channelAssociationRef(): ChannelAssociationReference;
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
 * Properties for defining a `CfnChannelAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-channelassociation.html
 */
export interface CfnChannelAssociationProps {
    /**
     * The Amazon Resource Name (ARN) of the `Channel` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-channelassociation.html#cfn-notifications-channelassociation-arn
     */
    readonly arn: string;
    /**
     * The ARN of the `NotificationConfiguration` associated with the `Channel` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-channelassociation.html#cfn-notifications-channelassociation-notificationconfigurationarn
     */
    readonly notificationConfigurationArn: string;
}
/**
 * A reference to a ChannelAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ChannelAssociationReference {
    /**
     * The Arn of the ChannelAssociation resource.
     */
    readonly channelAssociationArn: string;
    /**
     * The NotificationConfigurationArn of the ChannelAssociation resource.
     */
    readonly notificationConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a EventRule.
 *
 * @stability experimental
 */
export interface IEventRuleRef extends constructs.IConstruct {
    /**
     * A reference to a EventRule resource.
     */
    readonly eventRuleRef: EventRuleReference;
}
/**
 * Creates an [`EventRule`](https://docs.aws.amazon.com/notifications/latest/userguide/glossary.html) that is associated with a specified `NotificationConfiguration` .
 *
 * @cloudformationResource AWS::Notifications::EventRule
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html
 */
export declare class CfnEventRule extends cdk.CfnResource implements cdk.IInspectable, IEventRuleRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnEventRule from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnEventRule;
    /**
     * The Amazon Resource Name (ARN) of the `EventRule` . AWS CloudFormation stack generates this ARN and then uses this ARN associated with the `NotificationConfiguration` .
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The creation time of the `EventRule` .
     *
     * @cloudformationAttribute CreationTime
     */
    readonly attrCreationTime: string;
    /**
     * A list of Amazon EventBridge Managed Rule ARNs associated with this `EventRule` .
     *
     * > These are created by AWS User Notifications within your account so your `EventRules` can function.
     *
     * @cloudformationAttribute ManagedRules
     */
    readonly attrManagedRules: Array<string>;
    /**
     * @cloudformationAttribute StatusSummaryByRegion
     */
    readonly attrStatusSummaryByRegion: cdk.IResolvable;
    /**
     * An additional event pattern used to further filter the events this `EventRule` receives.
     */
    eventPattern?: string;
    /**
     * The event type this rule should match with the EventBridge events.
     */
    eventType: string;
    /**
     * The ARN for the `NotificationConfiguration` associated with this `EventRule` .
     */
    notificationConfigurationArn: string;
    /**
     * A list of AWS Regions that send events to this `EventRule` .
     */
    regions: Array<string>;
    /**
     * The event source this rule should match with the EventBridge event sources.
     */
    source: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnEventRuleProps);
    get eventRuleRef(): EventRuleReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnEventRule {
    /**
     * Provides additional information about the current `EventRule` status.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-eventrule-eventrulestatussummary.html
     */
    interface EventRuleStatusSummaryProperty {
        /**
         * A human-readable reason for `EventRuleStatus` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-eventrule-eventrulestatussummary.html#cfn-notifications-eventrule-eventrulestatussummary-reason
         */
        readonly reason: string;
        /**
         * The status of the `EventRule` .
         *
         * - Values:
         *
         * - `ACTIVE`
         *
         * - The `EventRule` can process events.
         * - `INACTIVE`
         *
         * - The `EventRule` may be unable to process events.
         * - `CREATING`
         *
         * - The `EventRule` is being created.
         *
         * Only `GET` and `LIST` calls can be run.
         * - `UPDATING`
         *
         * - The `EventRule` is being updated.
         *
         * Only `GET` and `LIST` calls can be run.
         * - `DELETING`
         *
         * - The `EventRule` is being deleted.
         *
         * Only `GET` and `LIST` calls can be run.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-eventrule-eventrulestatussummary.html#cfn-notifications-eventrule-eventrulestatussummary-status
         */
        readonly status: string;
    }
}
/**
 * Properties for defining a `CfnEventRule`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html
 */
export interface CfnEventRuleProps {
    /**
     * An additional event pattern used to further filter the events this `EventRule` receives.
     *
     * For more information, see [Amazon EventBridge event patterns](https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-patterns.html) in the *Amazon EventBridge User Guide.*
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html#cfn-notifications-eventrule-eventpattern
     */
    readonly eventPattern?: string;
    /**
     * The event type this rule should match with the EventBridge events.
     *
     * It must match with atleast one of the valid EventBridge event types. For example, Amazon EC2 Instance State change Notification and Amazon CloudWatch State Change. For more information, see [Event delivery from AWS services](https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-service-event.html#eb-service-event-delivery-level) in the *Amazon EventBridge User Guide* .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html#cfn-notifications-eventrule-eventtype
     */
    readonly eventType: string;
    /**
     * The ARN for the `NotificationConfiguration` associated with this `EventRule` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html#cfn-notifications-eventrule-notificationconfigurationarn
     */
    readonly notificationConfigurationArn: string;
    /**
     * A list of AWS Regions that send events to this `EventRule` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html#cfn-notifications-eventrule-regions
     */
    readonly regions: Array<string>;
    /**
     * The event source this rule should match with the EventBridge event sources.
     *
     * It must match with atleast one of the valid EventBridge event sources. Only AWS service sourced events are supported. For example, `aws.ec2` and `aws.cloudwatch` . For more information, see [Event delivery from AWS services](https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-service-event.html#eb-service-event-delivery-level) in the *Amazon EventBridge User Guide* .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-eventrule.html#cfn-notifications-eventrule-source
     */
    readonly source: string;
}
/**
 * A reference to a EventRule resource.
 *
 * @struct
 * @stability external
 */
export interface EventRuleReference {
    /**
     * The Arn of the EventRule resource.
     */
    readonly eventRuleArn: string;
}
/**
 * Indicates that this resource can be referenced as a ManagedNotificationAccountContactAssociation.
 *
 * @stability experimental
 */
export interface IManagedNotificationAccountContactAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ManagedNotificationAccountContactAssociation resource.
     */
    readonly managedNotificationAccountContactAssociationRef: ManagedNotificationAccountContactAssociationReference;
}
/**
 * Associates an Account Management Contact with a `ManagedNotificationConfiguration` for AWS User Notifications .
 *
 * For more information about AWS User Notifications , see the [AWS User Notifications User Guide](https://docs.aws.amazon.com/notifications/latest/userguide/what-is-service.html) . For more information about Account Management Contacts, see the [Account Management Reference Guide](https://docs.aws.amazon.com/accounts/latest/reference/API_AlternateContact.html) .
 *
 * @cloudformationResource AWS::Notifications::ManagedNotificationAccountContactAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationaccountcontactassociation.html
 */
export declare class CfnManagedNotificationAccountContactAssociation extends cdk.CfnResource implements cdk.IInspectable, IManagedNotificationAccountContactAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnManagedNotificationAccountContactAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnManagedNotificationAccountContactAssociation;
    /**
     * The unique identifier of the notification contact associated with the AWS account.
     */
    contactIdentifier: string;
    /**
     * The ARN of the `ManagedNotificationConfiguration` to be associated with the `Channel` .
     */
    managedNotificationConfigurationArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnManagedNotificationAccountContactAssociationProps);
    get managedNotificationAccountContactAssociationRef(): ManagedNotificationAccountContactAssociationReference;
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
 * Properties for defining a `CfnManagedNotificationAccountContactAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationaccountcontactassociation.html
 */
export interface CfnManagedNotificationAccountContactAssociationProps {
    /**
     * The unique identifier of the notification contact associated with the AWS account.
     *
     * For more information about the contact types associated with an account, see the [Account Management Reference Guide](https://docs.aws.amazon.com/accounts/latest/reference/manage-acct-update-contact-alternate.html#manage-acct-update-contact-alternate-orgs) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationaccountcontactassociation.html#cfn-notifications-managednotificationaccountcontactassociation-contactidentifier
     */
    readonly contactIdentifier: string;
    /**
     * The ARN of the `ManagedNotificationConfiguration` to be associated with the `Channel` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationaccountcontactassociation.html#cfn-notifications-managednotificationaccountcontactassociation-managednotificationconfigurationarn
     */
    readonly managedNotificationConfigurationArn: string;
}
/**
 * A reference to a ManagedNotificationAccountContactAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ManagedNotificationAccountContactAssociationReference {
    /**
     * The ManagedNotificationConfigurationArn of the ManagedNotificationAccountContactAssociation resource.
     */
    readonly managedNotificationConfigurationArn: string;
    /**
     * The ContactIdentifier of the ManagedNotificationAccountContactAssociation resource.
     */
    readonly contactIdentifier: string;
}
/**
 * Indicates that this resource can be referenced as a ManagedNotificationAdditionalChannelAssociation.
 *
 * @stability experimental
 */
export interface IManagedNotificationAdditionalChannelAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ManagedNotificationAdditionalChannelAssociation resource.
     */
    readonly managedNotificationAdditionalChannelAssociationRef: ManagedNotificationAdditionalChannelAssociationReference;
}
/**
 * Associates a `Channel` with a `ManagedNotificationAdditionalChannelAssociation` for AWS User Notifications .
 *
 * For more information about AWS User Notifications , see the [AWS User Notifications User Guide](https://docs.aws.amazon.com/notifications/latest/userguide/what-is-service.html) .
 *
 * @cloudformationResource AWS::Notifications::ManagedNotificationAdditionalChannelAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationadditionalchannelassociation.html
 */
export declare class CfnManagedNotificationAdditionalChannelAssociation extends cdk.CfnResource implements cdk.IInspectable, IManagedNotificationAdditionalChannelAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnManagedNotificationAdditionalChannelAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnManagedNotificationAdditionalChannelAssociation;
    /**
     * The ARN of the `Channel` .
     */
    channelArn: string;
    /**
     * The ARN of the `ManagedNotificationAdditionalChannelAssociation` associated with the `Channel` .
     */
    managedNotificationConfigurationArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnManagedNotificationAdditionalChannelAssociationProps);
    get managedNotificationAdditionalChannelAssociationRef(): ManagedNotificationAdditionalChannelAssociationReference;
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
 * Properties for defining a `CfnManagedNotificationAdditionalChannelAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationadditionalchannelassociation.html
 */
export interface CfnManagedNotificationAdditionalChannelAssociationProps {
    /**
     * The ARN of the `Channel` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationadditionalchannelassociation.html#cfn-notifications-managednotificationadditionalchannelassociation-channelarn
     */
    readonly channelArn: string;
    /**
     * The ARN of the `ManagedNotificationAdditionalChannelAssociation` associated with the `Channel` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-managednotificationadditionalchannelassociation.html#cfn-notifications-managednotificationadditionalchannelassociation-managednotificationconfigurationarn
     */
    readonly managedNotificationConfigurationArn: string;
}
/**
 * A reference to a ManagedNotificationAdditionalChannelAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ManagedNotificationAdditionalChannelAssociationReference {
    /**
     * The ChannelArn of the ManagedNotificationAdditionalChannelAssociation resource.
     */
    readonly channelArn: string;
    /**
     * The ManagedNotificationConfigurationArn of the ManagedNotificationAdditionalChannelAssociation resource.
     */
    readonly managedNotificationConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a NotificationConfiguration.
 *
 * @stability experimental
 */
export interface INotificationConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a NotificationConfiguration resource.
     */
    readonly notificationConfigurationRef: NotificationConfigurationReference;
}
/**
 * Configures a `NotificationConfiguration` for AWS User Notifications .
 *
 * @cloudformationResource AWS::Notifications::NotificationConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html
 */
export declare class CfnNotificationConfiguration extends cdk.CfnResource implements cdk.IInspectable, INotificationConfigurationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnNotificationConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnNotificationConfiguration;
    /**
     * The Amazon Resource Name (ARN) of the `NotificationConfiguration` resource.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The creation time of the `NotificationConfiguration` .
     *
     * @cloudformationAttribute CreationTime
     */
    readonly attrCreationTime: string;
    /**
     * The current status of the `NotificationConfiguration` .
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The aggregation preference of the `NotificationConfiguration` .
     */
    aggregationDuration?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The description of the `NotificationConfiguration` .
     */
    description: string;
    /**
     * The name of the `NotificationConfiguration` .
     */
    name: string;
    /**
     * A map of tags assigned to a `NotificationConfiguration` .
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnNotificationConfigurationProps);
    get notificationConfigurationRef(): NotificationConfigurationReference;
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
 * Properties for defining a `CfnNotificationConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html
 */
export interface CfnNotificationConfigurationProps {
    /**
     * The aggregation preference of the `NotificationConfiguration` .
     *
     * - Values:
     *
     * - `LONG`
     *
     * - Aggregate notifications for long periods of time (12 hours).
     * - `SHORT`
     *
     * - Aggregate notifications for short periods of time (5 minutes).
     * - `NONE`
     *
     * - Don't aggregate notifications.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html#cfn-notifications-notificationconfiguration-aggregationduration
     */
    readonly aggregationDuration?: string;
    /**
     * The description of the `NotificationConfiguration` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html#cfn-notifications-notificationconfiguration-description
     */
    readonly description: string;
    /**
     * The name of the `NotificationConfiguration` .
     *
     * Supports RFC 3986's unreserved characters.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html#cfn-notifications-notificationconfiguration-name
     */
    readonly name: string;
    /**
     * A map of tags assigned to a `NotificationConfiguration` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationconfiguration.html#cfn-notifications-notificationconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a NotificationConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface NotificationConfigurationReference {
    /**
     * The Arn of the NotificationConfiguration resource.
     */
    readonly notificationConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a NotificationHub.
 *
 * @stability experimental
 */
export interface INotificationHubRef extends constructs.IConstruct {
    /**
     * A reference to a NotificationHub resource.
     */
    readonly notificationHubRef: NotificationHubReference;
}
/**
 * Configures a `NotificationHub` for AWS User Notifications .
 *
 * For more information about notification hub, see the [AWS User Notifications User Guide](https://docs.aws.amazon.com/notifications/latest/userguide/notification-hubs.html) .
 *
 * @cloudformationResource AWS::Notifications::NotificationHub
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationhub.html
 */
export declare class CfnNotificationHub extends cdk.CfnResource implements cdk.IInspectable, INotificationHubRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnNotificationHub from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnNotificationHub;
    /**
     * The date and time the `NotificationHubOverview` was created.
     *
     * @cloudformationAttribute CreationTime
     */
    readonly attrCreationTime: string;
    /**
     * @cloudformationAttribute NotificationHubStatusSummary
     */
    readonly attrNotificationHubStatusSummary: cdk.IResolvable;
    /**
     * The `NotificationHub` Region.
     */
    region: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnNotificationHubProps);
    get notificationHubRef(): NotificationHubReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnNotificationHub {
    /**
     * Provides additional information about the current `NotificationHub` status.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-notificationhub-notificationhubstatussummary.html
     */
    interface NotificationHubStatusSummaryProperty {
        /**
         * Indicates the current status of the `NotificationHub` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-notificationhub-notificationhubstatussummary.html#cfn-notifications-notificationhub-notificationhubstatussummary-notificationhubstatus
         */
        readonly notificationHubStatus: string;
        /**
         * An explanation for the current status.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-notifications-notificationhub-notificationhubstatussummary.html#cfn-notifications-notificationhub-notificationhubstatussummary-notificationhubstatusreason
         */
        readonly notificationHubStatusReason: string;
    }
}
/**
 * Properties for defining a `CfnNotificationHub`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationhub.html
 */
export interface CfnNotificationHubProps {
    /**
     * The `NotificationHub` Region.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-notificationhub.html#cfn-notifications-notificationhub-region
     */
    readonly region: string;
}
/**
 * A reference to a NotificationHub resource.
 *
 * @struct
 * @stability external
 */
export interface NotificationHubReference {
    /**
     * The Region of the NotificationHub resource.
     */
    readonly region: string;
}
/**
 * Indicates that this resource can be referenced as a OrganizationalUnitAssociation.
 *
 * @stability experimental
 */
export interface IOrganizationalUnitAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a OrganizationalUnitAssociation resource.
     */
    readonly organizationalUnitAssociationRef: OrganizationalUnitAssociationReference;
}
/**
 * Resource Type definition for AWS::Notifications::OrganizationalUnitAssociation.
 *
 * @cloudformationResource AWS::Notifications::OrganizationalUnitAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-organizationalunitassociation.html
 */
export declare class CfnOrganizationalUnitAssociation extends cdk.CfnResource implements cdk.IInspectable, IOrganizationalUnitAssociationRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnOrganizationalUnitAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnOrganizationalUnitAssociation;
    /**
     * ARN identifier of the NotificationConfiguration.
     */
    notificationConfigurationArn: string;
    /**
     * The ID of the organizational unit.
     */
    organizationalUnitId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnOrganizationalUnitAssociationProps);
    get organizationalUnitAssociationRef(): OrganizationalUnitAssociationReference;
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
 * Properties for defining a `CfnOrganizationalUnitAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-organizationalunitassociation.html
 */
export interface CfnOrganizationalUnitAssociationProps {
    /**
     * ARN identifier of the NotificationConfiguration.
     *
     * Example: arn:aws:notifications::123456789012:configuration/a01jes88qxwkbj05xv9c967pgm1
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-organizationalunitassociation.html#cfn-notifications-organizationalunitassociation-notificationconfigurationarn
     */
    readonly notificationConfigurationArn: string;
    /**
     * The ID of the organizational unit.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-notifications-organizationalunitassociation.html#cfn-notifications-organizationalunitassociation-organizationalunitid
     */
    readonly organizationalUnitId: string;
}
/**
 * A reference to a OrganizationalUnitAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface OrganizationalUnitAssociationReference {
    /**
     * The NotificationConfigurationArn of the OrganizationalUnitAssociation resource.
     */
    readonly notificationConfigurationArn: string;
    /**
     * The OrganizationalUnitId of the OrganizationalUnitAssociation resource.
     */
    readonly organizationalUnitId: string;
}
