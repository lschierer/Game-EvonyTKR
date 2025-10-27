import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a AccessLogSubscription.
 *
 * @stability experimental
 */
export interface IAccessLogSubscriptionRef extends constructs.IConstruct {
    /**
     * A reference to a AccessLogSubscription resource.
     */
    readonly accessLogSubscriptionRef: AccessLogSubscriptionReference;
}
/**
 * Enables access logs to be sent to Amazon CloudWatch, Amazon S3, and Amazon Kinesis Data Firehose.
 *
 * The service network owner can use the access logs to audit the services in the network. The service network owner can only see access logs from clients and services that are associated with their service network. Access log entries represent traffic originated from VPCs associated with that network. For more information, see [Access logs](https://docs.aws.amazon.com/vpc-lattice/latest/ug/monitoring-access-logs.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::AccessLogSubscription
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html
 */
export declare class CfnAccessLogSubscription extends cdk.CfnResource implements cdk.IInspectable, IAccessLogSubscriptionRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnAccessLogSubscription from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnAccessLogSubscription;
    /**
     * The Amazon Resource Name (ARN) of the access log subscription.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the access log subscription.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The Amazon Resource Name (ARN) of the access log subscription.
     *
     * @cloudformationAttribute ResourceArn
     */
    readonly attrResourceArn: string;
    /**
     * The ID of the service network or service.
     *
     * @cloudformationAttribute ResourceId
     */
    readonly attrResourceId: string;
    /**
     * The Amazon Resource Name (ARN) of the destination.
     */
    destinationArn: string;
    /**
     * The ID or ARN of the service network or service.
     */
    resourceIdentifier?: string;
    /**
     * Log type of the service network.
     */
    serviceNetworkLogType?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the access log subscription.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnAccessLogSubscriptionProps);
    get accessLogSubscriptionRef(): AccessLogSubscriptionReference;
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
 * Properties for defining a `CfnAccessLogSubscription`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html
 */
export interface CfnAccessLogSubscriptionProps {
    /**
     * The Amazon Resource Name (ARN) of the destination.
     *
     * The supported destination types are CloudWatch Log groups, Kinesis Data Firehose delivery streams, and Amazon S3 buckets.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html#cfn-vpclattice-accesslogsubscription-destinationarn
     */
    readonly destinationArn: string;
    /**
     * The ID or ARN of the service network or service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html#cfn-vpclattice-accesslogsubscription-resourceidentifier
     */
    readonly resourceIdentifier?: string;
    /**
     * Log type of the service network.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html#cfn-vpclattice-accesslogsubscription-servicenetworklogtype
     */
    readonly serviceNetworkLogType?: string;
    /**
     * The tags for the access log subscription.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-accesslogsubscription.html#cfn-vpclattice-accesslogsubscription-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a AccessLogSubscription resource.
 *
 * @struct
 * @stability external
 */
export interface AccessLogSubscriptionReference {
    /**
     * The Arn of the AccessLogSubscription resource.
     */
    readonly accessLogSubscriptionArn: string;
}
/**
 * Indicates that this resource can be referenced as a AuthPolicy.
 *
 * @stability experimental
 */
export interface IAuthPolicyRef extends constructs.IConstruct {
    /**
     * A reference to a AuthPolicy resource.
     */
    readonly authPolicyRef: AuthPolicyReference;
}
/**
 * Creates or updates the auth policy. The policy string in JSON must not contain newlines or blank lines.
 *
 * For more information, see [Auth policies](https://docs.aws.amazon.com/vpc-lattice/latest/ug/auth-policies.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::AuthPolicy
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-authpolicy.html
 */
export declare class CfnAuthPolicy extends cdk.CfnResource implements cdk.IInspectable, IAuthPolicyRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnAuthPolicy from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnAuthPolicy;
    /**
     * The state of the auth policy. The auth policy is only active when the auth type is set to `AWS _IAM` . If you provide a policy, then authentication and authorization decisions are made based on this policy and the client's IAM policy. If the auth type is `NONE` , then any auth policy you provide will remain inactive.
     *
     * @cloudformationAttribute State
     */
    readonly attrState: string;
    /**
     * The auth policy.
     */
    policy: any | cdk.IResolvable;
    /**
     * The ID or ARN of the service network or service for which the policy is created.
     */
    resourceIdentifier: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnAuthPolicyProps);
    get authPolicyRef(): AuthPolicyReference;
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
 * Properties for defining a `CfnAuthPolicy`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-authpolicy.html
 */
export interface CfnAuthPolicyProps {
    /**
     * The auth policy.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-authpolicy.html#cfn-vpclattice-authpolicy-policy
     */
    readonly policy: any | cdk.IResolvable;
    /**
     * The ID or ARN of the service network or service for which the policy is created.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-authpolicy.html#cfn-vpclattice-authpolicy-resourceidentifier
     */
    readonly resourceIdentifier: string;
}
/**
 * A reference to a AuthPolicy resource.
 *
 * @struct
 * @stability external
 */
export interface AuthPolicyReference {
    /**
     * The ResourceIdentifier of the AuthPolicy resource.
     */
    readonly resourceIdentifier: string;
}
/**
 * Indicates that this resource can be referenced as a Listener.
 *
 * @stability experimental
 */
export interface IListenerRef extends constructs.IConstruct {
    /**
     * A reference to a Listener resource.
     */
    readonly listenerRef: ListenerReference;
}
/**
 * Creates a listener for a service.
 *
 * Before you start using your Amazon VPC Lattice service, you must add one or more listeners. A listener is a process that checks for connection requests to your services. For more information, see [Listeners](https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::Listener
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html
 */
export declare class CfnListener extends cdk.CfnResource implements cdk.IInspectable, IListenerRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnListener from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnListener;
    /**
     * The Amazon Resource Name (ARN) of the listener.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the listener.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The Amazon Resource Name (ARN) of the service.
     *
     * @cloudformationAttribute ServiceArn
     */
    readonly attrServiceArn: string;
    /**
     * The ID of the service.
     *
     * @cloudformationAttribute ServiceId
     */
    readonly attrServiceId: string;
    /**
     * The action for the default rule.
     */
    defaultAction: CfnListener.DefaultActionProperty | cdk.IResolvable;
    /**
     * The name of the listener.
     */
    name?: string;
    /**
     * The listener port.
     */
    port?: number;
    /**
     * The listener protocol.
     */
    protocol: string;
    /**
     * The ID or ARN of the service.
     */
    serviceIdentifier?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the listener.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnListenerProps);
    get listenerRef(): ListenerReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnListener {
    /**
     * The action for the default rule.
     *
     * Each listener has a default rule. The default rule is used if no other rules match.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-defaultaction.html
     */
    interface DefaultActionProperty {
        /**
         * Describes an action that returns a custom HTTP response.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-defaultaction.html#cfn-vpclattice-listener-defaultaction-fixedresponse
         */
        readonly fixedResponse?: CfnListener.FixedResponseProperty | cdk.IResolvable;
        /**
         * Describes a forward action.
         *
         * You can use forward actions to route requests to one or more target groups.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-defaultaction.html#cfn-vpclattice-listener-defaultaction-forward
         */
        readonly forward?: CfnListener.ForwardProperty | cdk.IResolvable;
    }
    /**
     * The forward action.
     *
     * Traffic that matches the rule is forwarded to the specified target groups.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-forward.html
     */
    interface ForwardProperty {
        /**
         * The target groups.
         *
         * Traffic matching the rule is forwarded to the specified target groups. With forward actions, you can assign a weight that controls the prioritization and selection of each target group. This means that requests are distributed to individual target groups based on their weights. For example, if two target groups have the same weight, each target group receives half of the traffic.
         *
         * The default value is 1. This means that if only one target group is provided, there is no need to set the weight; 100% of the traffic goes to that target group.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-forward.html#cfn-vpclattice-listener-forward-targetgroups
         */
        readonly targetGroups: Array<cdk.IResolvable | CfnListener.WeightedTargetGroupProperty> | cdk.IResolvable;
    }
    /**
     * Describes the weight of a target group.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-weightedtargetgroup.html
     */
    interface WeightedTargetGroupProperty {
        /**
         * The ID of the target group.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-weightedtargetgroup.html#cfn-vpclattice-listener-weightedtargetgroup-targetgroupidentifier
         */
        readonly targetGroupIdentifier: string;
        /**
         * Only required if you specify multiple target groups for a forward action.
         *
         * The weight determines how requests are distributed to the target group. For example, if you specify two target groups, each with a weight of 10, each target group receives half the requests. If you specify two target groups, one with a weight of 10 and the other with a weight of 20, the target group with a weight of 20 receives twice as many requests as the other target group. If there's only one target group specified, then the default value is 100.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-weightedtargetgroup.html#cfn-vpclattice-listener-weightedtargetgroup-weight
         */
        readonly weight?: number;
    }
    /**
     * Describes an action that returns a custom HTTP response.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-fixedresponse.html
     */
    interface FixedResponseProperty {
        /**
         * The HTTP response code.
         *
         * Only `404` and `500` status codes are supported.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-listener-fixedresponse.html#cfn-vpclattice-listener-fixedresponse-statuscode
         */
        readonly statusCode: number;
    }
}
/**
 * Properties for defining a `CfnListener`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html
 */
export interface CfnListenerProps {
    /**
     * The action for the default rule.
     *
     * Each listener has a default rule. The default rule is used if no other rules match.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-defaultaction
     */
    readonly defaultAction: CfnListener.DefaultActionProperty | cdk.IResolvable;
    /**
     * The name of the listener.
     *
     * A listener name must be unique within a service. The valid characters are a-z, 0-9, and hyphens (-). You can't use a hyphen as the first or last character, or immediately after another hyphen.
     *
     * If you don't specify a name, CloudFormation generates one. However, if you specify a name, and later want to replace the resource, you must specify a new name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-name
     */
    readonly name?: string;
    /**
     * The listener port.
     *
     * You can specify a value from 1 to 65535. For HTTP, the default is 80. For HTTPS, the default is 443.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-port
     */
    readonly port?: number;
    /**
     * The listener protocol.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-protocol
     */
    readonly protocol: string;
    /**
     * The ID or ARN of the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-serviceidentifier
     */
    readonly serviceIdentifier?: string;
    /**
     * The tags for the listener.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-listener.html#cfn-vpclattice-listener-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Listener resource.
 *
 * @struct
 * @stability external
 */
export interface ListenerReference {
    /**
     * The Arn of the Listener resource.
     */
    readonly listenerArn: string;
}
/**
 * Indicates that this resource can be referenced as a ResourcePolicy.
 *
 * @stability experimental
 */
export interface IResourcePolicyRef extends constructs.IConstruct {
    /**
     * A reference to a ResourcePolicy resource.
     */
    readonly resourcePolicyRef: ResourcePolicyReference;
}
/**
 * Retrieves information about the specified resource policy.
 *
 * The resource policy is an IAM policy created on behalf of the resource owner when they share a resource.
 *
 * @cloudformationResource AWS::VpcLattice::ResourcePolicy
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcepolicy.html
 */
export declare class CfnResourcePolicy extends cdk.CfnResource implements cdk.IInspectable, IResourcePolicyRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnResourcePolicy from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnResourcePolicy;
    /**
     * The Amazon Resource Name (ARN) of the service network or service.
     */
    policy: any | cdk.IResolvable;
    /**
     * An IAM policy.
     */
    resourceArn: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnResourcePolicyProps);
    get resourcePolicyRef(): ResourcePolicyReference;
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
 * Properties for defining a `CfnResourcePolicy`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcepolicy.html
 */
export interface CfnResourcePolicyProps {
    /**
     * The Amazon Resource Name (ARN) of the service network or service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcepolicy.html#cfn-vpclattice-resourcepolicy-policy
     */
    readonly policy: any | cdk.IResolvable;
    /**
     * An IAM policy.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcepolicy.html#cfn-vpclattice-resourcepolicy-resourcearn
     */
    readonly resourceArn: string;
}
/**
 * A reference to a ResourcePolicy resource.
 *
 * @struct
 * @stability external
 */
export interface ResourcePolicyReference {
    /**
     * The ResourceArn of the ResourcePolicy resource.
     */
    readonly resourceArn: string;
}
/**
 * Indicates that this resource can be referenced as a Rule.
 *
 * @stability experimental
 */
export interface IRuleRef extends constructs.IConstruct {
    /**
     * A reference to a Rule resource.
     */
    readonly ruleRef: RuleReference;
}
/**
 * Creates a listener rule.
 *
 * Each listener has a default rule for checking connection requests, but you can define additional rules. Each rule consists of a priority, one or more actions, and one or more conditions. For more information, see [Listener rules](https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html#listener-rules) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::Rule
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html
 */
export declare class CfnRule extends cdk.CfnResource implements cdk.IInspectable, IRuleRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnRule from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnRule;
    /**
     * The Amazon Resource Name (ARN) of the rule.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the listener.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * Describes the action for a rule.
     */
    action: CfnRule.ActionProperty | cdk.IResolvable;
    /**
     * The ID or ARN of the listener.
     */
    listenerIdentifier?: string;
    /**
     * The rule match.
     */
    match: cdk.IResolvable | CfnRule.MatchProperty;
    /**
     * The name of the rule.
     */
    name?: string;
    /**
     * The priority assigned to the rule.
     */
    priority: number;
    /**
     * The ID or ARN of the service.
     */
    serviceIdentifier?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the rule.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnRuleProps);
    get ruleRef(): RuleReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnRule {
    /**
     * Describes the action for a rule.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-action.html
     */
    interface ActionProperty {
        /**
         * The fixed response action.
         *
         * The rule returns a custom HTTP response.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-action.html#cfn-vpclattice-rule-action-fixedresponse
         */
        readonly fixedResponse?: CfnRule.FixedResponseProperty | cdk.IResolvable;
        /**
         * The forward action.
         *
         * Traffic that matches the rule is forwarded to the specified target groups.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-action.html#cfn-vpclattice-rule-action-forward
         */
        readonly forward?: CfnRule.ForwardProperty | cdk.IResolvable;
    }
    /**
     * The forward action.
     *
     * Traffic that matches the rule is forwarded to the specified target groups.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-forward.html
     */
    interface ForwardProperty {
        /**
         * The target groups.
         *
         * Traffic matching the rule is forwarded to the specified target groups. With forward actions, you can assign a weight that controls the prioritization and selection of each target group. This means that requests are distributed to individual target groups based on their weights. For example, if two target groups have the same weight, each target group receives half of the traffic.
         *
         * The default value is 1. This means that if only one target group is provided, there is no need to set the weight; 100% of the traffic goes to that target group.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-forward.html#cfn-vpclattice-rule-forward-targetgroups
         */
        readonly targetGroups: Array<cdk.IResolvable | CfnRule.WeightedTargetGroupProperty> | cdk.IResolvable;
    }
    /**
     * Describes the weight of a target group.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-weightedtargetgroup.html
     */
    interface WeightedTargetGroupProperty {
        /**
         * The ID of the target group.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-weightedtargetgroup.html#cfn-vpclattice-rule-weightedtargetgroup-targetgroupidentifier
         */
        readonly targetGroupIdentifier: string;
        /**
         * Only required if you specify multiple target groups for a forward action.
         *
         * The weight determines how requests are distributed to the target group. For example, if you specify two target groups, each with a weight of 10, each target group receives half the requests. If you specify two target groups, one with a weight of 10 and the other with a weight of 20, the target group with a weight of 20 receives twice as many requests as the other target group. If there's only one target group specified, then the default value is 100.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-weightedtargetgroup.html#cfn-vpclattice-rule-weightedtargetgroup-weight
         */
        readonly weight?: number;
    }
    /**
     * Describes an action that returns a custom HTTP response.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-fixedresponse.html
     */
    interface FixedResponseProperty {
        /**
         * The HTTP response code.
         *
         * Only `404` and `500` status codes are supported.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-fixedresponse.html#cfn-vpclattice-rule-fixedresponse-statuscode
         */
        readonly statusCode: number;
    }
    /**
     * Describes a rule match.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-match.html
     */
    interface MatchProperty {
        /**
         * The HTTP criteria that a rule must match.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-match.html#cfn-vpclattice-rule-match-httpmatch
         */
        readonly httpMatch: CfnRule.HttpMatchProperty | cdk.IResolvable;
    }
    /**
     * Describes criteria that can be applied to incoming requests.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-httpmatch.html
     */
    interface HttpMatchProperty {
        /**
         * The header matches.
         *
         * Matches incoming requests with rule based on request header value before applying rule action.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-httpmatch.html#cfn-vpclattice-rule-httpmatch-headermatches
         */
        readonly headerMatches?: Array<CfnRule.HeaderMatchProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * The HTTP method type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-httpmatch.html#cfn-vpclattice-rule-httpmatch-method
         */
        readonly method?: string;
        /**
         * The path match.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-httpmatch.html#cfn-vpclattice-rule-httpmatch-pathmatch
         */
        readonly pathMatch?: cdk.IResolvable | CfnRule.PathMatchProperty;
    }
    /**
     * Describes the constraints for a header match.
     *
     * Matches incoming requests with rule based on request header value before applying rule action.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatch.html
     */
    interface HeaderMatchProperty {
        /**
         * Indicates whether the match is case sensitive.
         *
         * @default - false
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatch.html#cfn-vpclattice-rule-headermatch-casesensitive
         */
        readonly caseSensitive?: boolean | cdk.IResolvable;
        /**
         * The header match type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatch.html#cfn-vpclattice-rule-headermatch-match
         */
        readonly match: CfnRule.HeaderMatchTypeProperty | cdk.IResolvable;
        /**
         * The name of the header.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatch.html#cfn-vpclattice-rule-headermatch-name
         */
        readonly name: string;
    }
    /**
     * Describes a header match type.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatchtype.html
     */
    interface HeaderMatchTypeProperty {
        /**
         * A contains type match.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatchtype.html#cfn-vpclattice-rule-headermatchtype-contains
         */
        readonly contains?: string;
        /**
         * An exact type match.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatchtype.html#cfn-vpclattice-rule-headermatchtype-exact
         */
        readonly exact?: string;
        /**
         * A prefix type match.
         *
         * Matches the value with the prefix.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-headermatchtype.html#cfn-vpclattice-rule-headermatchtype-prefix
         */
        readonly prefix?: string;
    }
    /**
     * Describes the conditions that can be applied when matching a path for incoming requests.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatch.html
     */
    interface PathMatchProperty {
        /**
         * Indicates whether the match is case sensitive.
         *
         * @default - false
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatch.html#cfn-vpclattice-rule-pathmatch-casesensitive
         */
        readonly caseSensitive?: boolean | cdk.IResolvable;
        /**
         * The type of path match.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatch.html#cfn-vpclattice-rule-pathmatch-match
         */
        readonly match: cdk.IResolvable | CfnRule.PathMatchTypeProperty;
    }
    /**
     * Describes a path match type.
     *
     * Each rule can include only one of the following types of paths.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatchtype.html
     */
    interface PathMatchTypeProperty {
        /**
         * An exact match of the path.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatchtype.html#cfn-vpclattice-rule-pathmatchtype-exact
         */
        readonly exact?: string;
        /**
         * A prefix match of the path.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-rule-pathmatchtype.html#cfn-vpclattice-rule-pathmatchtype-prefix
         */
        readonly prefix?: string;
    }
}
/**
 * Properties for defining a `CfnRule`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html
 */
export interface CfnRuleProps {
    /**
     * Describes the action for a rule.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-action
     */
    readonly action: CfnRule.ActionProperty | cdk.IResolvable;
    /**
     * The ID or ARN of the listener.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-listeneridentifier
     */
    readonly listenerIdentifier?: string;
    /**
     * The rule match.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-match
     */
    readonly match: cdk.IResolvable | CfnRule.MatchProperty;
    /**
     * The name of the rule.
     *
     * The name must be unique within the listener. The valid characters are a-z, 0-9, and hyphens (-). You can't use a hyphen as the first or last character, or immediately after another hyphen.
     *
     * If you don't specify a name, CloudFormation generates one. However, if you specify a name, and later want to replace the resource, you must specify a new name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-name
     */
    readonly name?: string;
    /**
     * The priority assigned to the rule.
     *
     * Each rule for a specific listener must have a unique priority. The lower the priority number the higher the priority.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-priority
     */
    readonly priority: number;
    /**
     * The ID or ARN of the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-serviceidentifier
     */
    readonly serviceIdentifier?: string;
    /**
     * The tags for the rule.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-rule.html#cfn-vpclattice-rule-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Rule resource.
 *
 * @struct
 * @stability external
 */
export interface RuleReference {
    /**
     * The Arn of the Rule resource.
     */
    readonly ruleArn: string;
}
/**
 * Indicates that this resource can be referenced as a Service.
 *
 * @stability experimental
 */
export interface IServiceRef extends constructs.IConstruct {
    /**
     * A reference to a Service resource.
     */
    readonly serviceRef: ServiceReference;
}
/**
 * Creates a service.
 *
 * A service is any software application that can run on instances containers, or serverless functions within an account or virtual private cloud (VPC).
 *
 * For more information, see [Services](https://docs.aws.amazon.com/vpc-lattice/latest/ug/services.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::Service
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html
 */
export declare class CfnService extends cdk.CfnResource implements cdk.IInspectable, IServiceRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnService from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnService;
    /**
     * The Amazon Resource Name (ARN) of the service.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The date and time that the service was created, specified in ISO-8601 format.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: string;
    /**
     * The domain name of the service.
     *
     * @cloudformationAttribute DnsEntry.DomainName
     */
    readonly attrDnsEntryDomainName: string;
    /**
     * The ID of the hosted zone.
     *
     * @cloudformationAttribute DnsEntry.HostedZoneId
     */
    readonly attrDnsEntryHostedZoneId: string;
    /**
     * The ID of the service.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The date and time that the service was last updated, specified in ISO-8601 format.
     *
     * @cloudformationAttribute LastUpdatedAt
     */
    readonly attrLastUpdatedAt: string;
    /**
     * The status of the service.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The type of IAM policy.
     */
    authType?: string;
    /**
     * The Amazon Resource Name (ARN) of the certificate.
     */
    certificateArn?: string;
    /**
     * The custom domain name of the service.
     */
    customDomainName?: string;
    /**
     * Describes the DNS information of the service.
     */
    dnsEntry?: CfnService.DnsEntryProperty | cdk.IResolvable;
    /**
     * The name of the service.
     */
    name?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the service.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnServiceProps);
    get serviceRef(): ServiceReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnService {
    /**
     * Describes the DNS information of a service.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-service-dnsentry.html
     */
    interface DnsEntryProperty {
        /**
         * The domain name of the service.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-service-dnsentry.html#cfn-vpclattice-service-dnsentry-domainname
         */
        readonly domainName?: string;
        /**
         * The ID of the hosted zone.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-service-dnsentry.html#cfn-vpclattice-service-dnsentry-hostedzoneid
         */
        readonly hostedZoneId?: string;
    }
}
/**
 * Properties for defining a `CfnService`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html
 */
export interface CfnServiceProps {
    /**
     * The type of IAM policy.
     *
     * - `NONE` : The resource does not use an IAM policy. This is the default.
     * - `AWS_IAM` : The resource uses an IAM policy. When this type is used, auth is enabled and an auth policy is required.
     *
     * @default - "NONE"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-authtype
     */
    readonly authType?: string;
    /**
     * The Amazon Resource Name (ARN) of the certificate.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-certificatearn
     */
    readonly certificateArn?: string;
    /**
     * The custom domain name of the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-customdomainname
     */
    readonly customDomainName?: string;
    /**
     * Describes the DNS information of the service.
     *
     * This field is read-only.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-dnsentry
     */
    readonly dnsEntry?: CfnService.DnsEntryProperty | cdk.IResolvable;
    /**
     * The name of the service.
     *
     * The name must be unique within the account. The valid characters are a-z, 0-9, and hyphens (-). You can't use a hyphen as the first or last character, or immediately after another hyphen.
     *
     * If you don't specify a name, CloudFormation generates one. However, if you specify a name, and later want to replace the resource, you must specify a new name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-name
     */
    readonly name?: string;
    /**
     * The tags for the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-service.html#cfn-vpclattice-service-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Service resource.
 *
 * @struct
 * @stability external
 */
export interface ServiceReference {
    /**
     * The Arn of the Service resource.
     */
    readonly serviceArn: string;
}
/**
 * Indicates that this resource can be referenced as a ServiceNetwork.
 *
 * @stability experimental
 */
export interface IServiceNetworkRef extends constructs.IConstruct {
    /**
     * A reference to a ServiceNetwork resource.
     */
    readonly serviceNetworkRef: ServiceNetworkReference;
}
/**
 * Creates a service network.
 *
 * A service network is a logical boundary for a collection of services. You can associate services and VPCs with a service network.
 *
 * For more information, see [Service networks](https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::ServiceNetwork
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html
 */
export declare class CfnServiceNetwork extends cdk.CfnResource implements cdk.IInspectable, IServiceNetworkRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnServiceNetwork from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnServiceNetwork;
    /**
     * The Amazon Resource Name (ARN) of the service network.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The date and time that the service network was created, specified in ISO-8601 format.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: string;
    /**
     * The ID of the service network.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The date and time of the last update, specified in ISO-8601 format.
     *
     * @cloudformationAttribute LastUpdatedAt
     */
    readonly attrLastUpdatedAt: string;
    /**
     * The type of IAM policy.
     */
    authType?: string;
    /**
     * The name of the service network.
     */
    name?: string;
    /**
     * Specify if the service network should be enabled for sharing.
     */
    sharingConfig?: cdk.IResolvable | CfnServiceNetwork.SharingConfigProperty;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the service network.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnServiceNetworkProps);
    get serviceNetworkRef(): ServiceNetworkReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnServiceNetwork {
    /**
     * Specify if the service network should be enabled for sharing.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-servicenetwork-sharingconfig.html
     */
    interface SharingConfigProperty {
        /**
         * Specify if the service network should be enabled for sharing.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-servicenetwork-sharingconfig.html#cfn-vpclattice-servicenetwork-sharingconfig-enabled
         */
        readonly enabled: boolean | cdk.IResolvable;
    }
}
/**
 * Properties for defining a `CfnServiceNetwork`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html
 */
export interface CfnServiceNetworkProps {
    /**
     * The type of IAM policy.
     *
     * - `NONE` : The resource does not use an IAM policy. This is the default.
     * - `AWS_IAM` : The resource uses an IAM policy. When this type is used, auth is enabled and an auth policy is required.
     *
     * @default - "NONE"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html#cfn-vpclattice-servicenetwork-authtype
     */
    readonly authType?: string;
    /**
     * The name of the service network.
     *
     * The name must be unique to the account. The valid characters are a-z, 0-9, and hyphens (-). You can't use a hyphen as the first or last character, or immediately after another hyphen.
     *
     * If you don't specify a name, CloudFormation generates one. However, if you specify a name, and later want to replace the resource, you must specify a new name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html#cfn-vpclattice-servicenetwork-name
     */
    readonly name?: string;
    /**
     * Specify if the service network should be enabled for sharing.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html#cfn-vpclattice-servicenetwork-sharingconfig
     */
    readonly sharingConfig?: cdk.IResolvable | CfnServiceNetwork.SharingConfigProperty;
    /**
     * The tags for the service network.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetwork.html#cfn-vpclattice-servicenetwork-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a ServiceNetwork resource.
 *
 * @struct
 * @stability external
 */
export interface ServiceNetworkReference {
    /**
     * The Arn of the ServiceNetwork resource.
     */
    readonly serviceNetworkArn: string;
}
/**
 * Indicates that this resource can be referenced as a ServiceNetworkServiceAssociation.
 *
 * @stability experimental
 */
export interface IServiceNetworkServiceAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ServiceNetworkServiceAssociation resource.
     */
    readonly serviceNetworkServiceAssociationRef: ServiceNetworkServiceAssociationReference;
}
/**
 * Associates the specified service with the specified service network.
 *
 * For more information, see [Manage service associations](https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-network-associations.html#service-network-service-associations) in the *Amazon VPC Lattice User Guide* .
 *
 * You can't use this operation if the service and service network are already associated or if there is a disassociation or deletion in progress. If the association fails, you can retry the operation by deleting the association and recreating it.
 *
 * You cannot associate a service and service network that are shared with a caller. The caller must own either the service or the service network.
 *
 * As a result of this operation, the association is created in the service network account and the association owner account.
 *
 * @cloudformationResource AWS::VpcLattice::ServiceNetworkServiceAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html
 */
export declare class CfnServiceNetworkServiceAssociation extends cdk.CfnResource implements cdk.IInspectable, IServiceNetworkServiceAssociationRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnServiceNetworkServiceAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnServiceNetworkServiceAssociation;
    /**
     * The Amazon Resource Name (ARN) of the association between the service network and the service.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The date and time that the association was created, specified in ISO-8601 format.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: string;
    /**
     * The domain name of the service.
     *
     * @cloudformationAttribute DnsEntry.DomainName
     */
    readonly attrDnsEntryDomainName: string;
    /**
     * The ID of the hosted zone.
     *
     * @cloudformationAttribute DnsEntry.HostedZoneId
     */
    readonly attrDnsEntryHostedZoneId: string;
    /**
     * The ID of the of the association between the service network and the service.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The Amazon Resource Name (ARN) of the service.
     *
     * @cloudformationAttribute ServiceArn
     */
    readonly attrServiceArn: string;
    /**
     * The ID of the service.
     *
     * @cloudformationAttribute ServiceId
     */
    readonly attrServiceId: string;
    /**
     * The name of the service.
     *
     * @cloudformationAttribute ServiceName
     */
    readonly attrServiceName: string;
    /**
     * The Amazon Resource Name (ARN) of the service network
     *
     * @cloudformationAttribute ServiceNetworkArn
     */
    readonly attrServiceNetworkArn: string;
    /**
     * The ID of the service network.
     *
     * @cloudformationAttribute ServiceNetworkId
     */
    readonly attrServiceNetworkId: string;
    /**
     * The name of the service network.
     *
     * @cloudformationAttribute ServiceNetworkName
     */
    readonly attrServiceNetworkName: string;
    /**
     * The status of the association between the service network and the service.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The DNS information of the service.
     */
    dnsEntry?: CfnServiceNetworkServiceAssociation.DnsEntryProperty | cdk.IResolvable;
    /**
     * The ID or ARN of the service.
     */
    serviceIdentifier?: string;
    /**
     * The ID or ARN of the service network.
     */
    serviceNetworkIdentifier?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the association.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnServiceNetworkServiceAssociationProps);
    get serviceNetworkServiceAssociationRef(): ServiceNetworkServiceAssociationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnServiceNetworkServiceAssociation {
    /**
     * The DNS information.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-servicenetworkserviceassociation-dnsentry.html
     */
    interface DnsEntryProperty {
        /**
         * The domain name of the service.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-servicenetworkserviceassociation-dnsentry.html#cfn-vpclattice-servicenetworkserviceassociation-dnsentry-domainname
         */
        readonly domainName?: string;
        /**
         * The ID of the hosted zone.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-servicenetworkserviceassociation-dnsentry.html#cfn-vpclattice-servicenetworkserviceassociation-dnsentry-hostedzoneid
         */
        readonly hostedZoneId?: string;
    }
}
/**
 * Properties for defining a `CfnServiceNetworkServiceAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html
 */
export interface CfnServiceNetworkServiceAssociationProps {
    /**
     * The DNS information of the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html#cfn-vpclattice-servicenetworkserviceassociation-dnsentry
     */
    readonly dnsEntry?: CfnServiceNetworkServiceAssociation.DnsEntryProperty | cdk.IResolvable;
    /**
     * The ID or ARN of the service.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html#cfn-vpclattice-servicenetworkserviceassociation-serviceidentifier
     */
    readonly serviceIdentifier?: string;
    /**
     * The ID or ARN of the service network.
     *
     * You must use an ARN if the resources are in different accounts.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html#cfn-vpclattice-servicenetworkserviceassociation-servicenetworkidentifier
     */
    readonly serviceNetworkIdentifier?: string;
    /**
     * The tags for the association.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkserviceassociation.html#cfn-vpclattice-servicenetworkserviceassociation-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a ServiceNetworkServiceAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ServiceNetworkServiceAssociationReference {
    /**
     * The Arn of the ServiceNetworkServiceAssociation resource.
     */
    readonly serviceNetworkServiceAssociationArn: string;
}
/**
 * Indicates that this resource can be referenced as a ServiceNetworkVpcAssociation.
 *
 * @stability experimental
 */
export interface IServiceNetworkVpcAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ServiceNetworkVpcAssociation resource.
     */
    readonly serviceNetworkVpcAssociationRef: ServiceNetworkVpcAssociationReference;
}
/**
 * Associates a VPC with a service network.
 *
 * When you associate a VPC with the service network, it enables all the resources within that VPC to be clients and communicate with other services in the service network. For more information, see [Manage VPC associations](https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-network-associations.html#service-network-vpc-associations) in the *Amazon VPC Lattice User Guide* .
 *
 * You can't use this operation if there is a disassociation in progress. If the association fails, retry by deleting the association and recreating it.
 *
 * As a result of this operation, the association gets created in the service network account and the VPC owner account.
 *
 * If you add a security group to the service network and VPC association, the association must continue to always have at least one security group. You can add or edit security groups at any time. However, to remove all security groups, you must first delete the association and recreate it without security groups.
 *
 * @cloudformationResource AWS::VpcLattice::ServiceNetworkVpcAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html
 */
export declare class CfnServiceNetworkVpcAssociation extends cdk.CfnResource implements cdk.IInspectable, IServiceNetworkVpcAssociationRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnServiceNetworkVpcAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnServiceNetworkVpcAssociation;
    /**
     * The Amazon Resource Name (ARN) of the association between the service network and the VPC.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The date and time that the association was created, specified in ISO-8601 format.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: string;
    /**
     * The ID of the specified association between the service network and the VPC.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The Amazon Resource Name (ARN) of the service network.
     *
     * @cloudformationAttribute ServiceNetworkArn
     */
    readonly attrServiceNetworkArn: string;
    /**
     * The ID of the service network.
     *
     * @cloudformationAttribute ServiceNetworkId
     */
    readonly attrServiceNetworkId: string;
    /**
     * The name of the service network.
     *
     * @cloudformationAttribute ServiceNetworkName
     */
    readonly attrServiceNetworkName: string;
    /**
     * The status of the association.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The ID of the VPC.
     *
     * @cloudformationAttribute VpcId
     */
    readonly attrVpcId: string;
    /**
     * The IDs of the security groups.
     */
    securityGroupIds?: Array<string>;
    /**
     * The ID or ARN of the service network.
     */
    serviceNetworkIdentifier?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the association.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * The ID of the VPC.
     */
    vpcIdentifier?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnServiceNetworkVpcAssociationProps);
    get serviceNetworkVpcAssociationRef(): ServiceNetworkVpcAssociationReference;
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
 * Properties for defining a `CfnServiceNetworkVpcAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html
 */
export interface CfnServiceNetworkVpcAssociationProps {
    /**
     * The IDs of the security groups.
     *
     * Security groups aren't added by default. You can add a security group to apply network level controls to control which resources in a VPC are allowed to access the service network and its services. For more information, see [Control traffic to resources using security groups](https://docs.aws.amazon.com//vpc/latest/userguide/VPC_SecurityGroups.html) in the *Amazon VPC User Guide* .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html#cfn-vpclattice-servicenetworkvpcassociation-securitygroupids
     */
    readonly securityGroupIds?: Array<string>;
    /**
     * The ID or ARN of the service network.
     *
     * You must use an ARN if the resources are in different accounts.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html#cfn-vpclattice-servicenetworkvpcassociation-servicenetworkidentifier
     */
    readonly serviceNetworkIdentifier?: string;
    /**
     * The tags for the association.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html#cfn-vpclattice-servicenetworkvpcassociation-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The ID of the VPC.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkvpcassociation.html#cfn-vpclattice-servicenetworkvpcassociation-vpcidentifier
     */
    readonly vpcIdentifier?: string;
}
/**
 * A reference to a ServiceNetworkVpcAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ServiceNetworkVpcAssociationReference {
    /**
     * The Arn of the ServiceNetworkVpcAssociation resource.
     */
    readonly serviceNetworkVpcAssociationArn: string;
}
/**
 * Indicates that this resource can be referenced as a TargetGroup.
 *
 * @stability experimental
 */
export interface ITargetGroupRef extends constructs.IConstruct {
    /**
     * A reference to a TargetGroup resource.
     */
    readonly targetGroupRef: TargetGroupReference;
}
/**
 * Creates a target group.
 *
 * A target group is a collection of targets, or compute resources, that run your application or service. A target group can only be used by a single service.
 *
 * For more information, see [Target groups](https://docs.aws.amazon.com/vpc-lattice/latest/ug/target-groups.html) in the *Amazon VPC Lattice User Guide* .
 *
 * @cloudformationResource AWS::VpcLattice::TargetGroup
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html
 */
export declare class CfnTargetGroup extends cdk.CfnResource implements cdk.IInspectable, ITargetGroupRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTargetGroup from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTargetGroup;
    /**
     * The Amazon Resource Name (ARN) of the target group.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The date and time that the target group was created, specified in ISO-8601 format.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: string;
    /**
     * The ID of the target group.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * The date and time that the target group was last updated, specified in ISO-8601 format.
     *
     * @cloudformationAttribute LastUpdatedAt
     */
    readonly attrLastUpdatedAt: string;
    /**
     * The operation's status. You can retry the operation if the status is `CREATE_FAILED` . However, if you retry it while the status is `CREATE_IN_PROGRESS` , there is no change in the status.
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * The target group configuration.
     */
    config?: cdk.IResolvable | CfnTargetGroup.TargetGroupConfigProperty;
    /**
     * The name of the target group.
     */
    name?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * The tags for the target group.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * Describes a target.
     */
    targets?: Array<cdk.IResolvable | CfnTargetGroup.TargetProperty> | cdk.IResolvable;
    /**
     * The type of target group.
     */
    type: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTargetGroupProps);
    get targetGroupRef(): TargetGroupReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnTargetGroup {
    /**
     * Describes the configuration of a target group.
     *
     * For more information, see [Target groups](https://docs.aws.amazon.com/vpc-lattice/latest/ug/target-groups.html) in the *Amazon VPC Lattice User Guide* .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html
     */
    interface TargetGroupConfigProperty {
        /**
         * The health check configuration.
         *
         * Not supported if the target group type is `LAMBDA` or `ALB` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-healthcheck
         */
        readonly healthCheck?: CfnTargetGroup.HealthCheckConfigProperty | cdk.IResolvable;
        /**
         * The type of IP address used for the target group.
         *
         * Supported only if the target group type is `IP` . The default is `IPV4` .
         *
         * @default - "IPV4"
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-ipaddresstype
         */
        readonly ipAddressType?: string;
        /**
         * The version of the event structure that your Lambda function receives.
         *
         * Supported only if the target group type is `LAMBDA` . The default is `V1` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-lambdaeventstructureversion
         */
        readonly lambdaEventStructureVersion?: string;
        /**
         * The port on which the targets are listening.
         *
         * For HTTP, the default is 80. For HTTPS, the default is 443. Not supported if the target group type is `LAMBDA` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-port
         */
        readonly port?: number;
        /**
         * The protocol to use for routing traffic to the targets.
         *
         * The default is the protocol of the target group. Not supported if the target group type is `LAMBDA` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-protocol
         */
        readonly protocol?: string;
        /**
         * The protocol version.
         *
         * The default is `HTTP1` . Not supported if the target group type is `LAMBDA` .
         *
         * @default - "HTTP1"
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-protocolversion
         */
        readonly protocolVersion?: string;
        /**
         * The ID of the VPC.
         *
         * Not supported if the target group type is `LAMBDA` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-targetgroupconfig.html#cfn-vpclattice-targetgroup-targetgroupconfig-vpcidentifier
         */
        readonly vpcIdentifier?: string;
    }
    /**
     * Describes the health check configuration of a target group.
     *
     * Health check configurations aren't used for target groups of type `LAMBDA` or `ALB` .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html
     */
    interface HealthCheckConfigProperty {
        /**
         * Indicates whether health checking is enabled.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-enabled
         */
        readonly enabled?: boolean | cdk.IResolvable;
        /**
         * The approximate amount of time, in seconds, between health checks of an individual target.
         *
         * The range is 5–300 seconds. The default is 30 seconds.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-healthcheckintervalseconds
         */
        readonly healthCheckIntervalSeconds?: number;
        /**
         * The amount of time, in seconds, to wait before reporting a target as unhealthy.
         *
         * The range is 1–120 seconds. The default is 5 seconds.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-healthchecktimeoutseconds
         */
        readonly healthCheckTimeoutSeconds?: number;
        /**
         * The number of consecutive successful health checks required before considering an unhealthy target healthy.
         *
         * The range is 2–10. The default is 5.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-healthythresholdcount
         */
        readonly healthyThresholdCount?: number;
        /**
         * The codes to use when checking for a successful response from a target.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-matcher
         */
        readonly matcher?: cdk.IResolvable | CfnTargetGroup.MatcherProperty;
        /**
         * The destination for health checks on the targets.
         *
         * If the protocol version is `HTTP/1.1` or `HTTP/2` , specify a valid URI (for example, `/path?query` ). The default path is `/` . Health checks are not supported if the protocol version is `gRPC` , however, you can choose `HTTP/1.1` or `HTTP/2` and specify a valid URI.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-path
         */
        readonly path?: string;
        /**
         * The port used when performing health checks on targets.
         *
         * The default setting is the port that a target receives traffic on.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-port
         */
        readonly port?: number;
        /**
         * The protocol used when performing health checks on targets.
         *
         * The possible protocols are `HTTP` and `HTTPS` . The default is `HTTP` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-protocol
         */
        readonly protocol?: string;
        /**
         * The protocol version used when performing health checks on targets.
         *
         * The possible protocol versions are `HTTP1` and `HTTP2` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-protocolversion
         */
        readonly protocolVersion?: string;
        /**
         * The number of consecutive failed health checks required before considering a target unhealthy.
         *
         * The range is 2–10. The default is 2.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-healthcheckconfig.html#cfn-vpclattice-targetgroup-healthcheckconfig-unhealthythresholdcount
         */
        readonly unhealthyThresholdCount?: number;
    }
    /**
     * Describes the codes to use when checking for a successful response from a target for health checks.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-matcher.html
     */
    interface MatcherProperty {
        /**
         * The HTTP code to use when checking for a successful response from a target.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-matcher.html#cfn-vpclattice-targetgroup-matcher-httpcode
         */
        readonly httpCode: string;
    }
    /**
     * Describes a target.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-target.html
     */
    interface TargetProperty {
        /**
         * The ID of the target.
         *
         * If the target group type is `INSTANCE` , this is an instance ID. If the target group type is `IP` , this is an IP address. If the target group type is `LAMBDA` , this is the ARN of a Lambda function. If the target group type is `ALB` , this is the ARN of an Application Load Balancer.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-target.html#cfn-vpclattice-targetgroup-target-id
         */
        readonly id: string;
        /**
         * The port on which the target is listening.
         *
         * For HTTP, the default is 80. For HTTPS, the default is 443.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-targetgroup-target.html#cfn-vpclattice-targetgroup-target-port
         */
        readonly port?: number;
    }
}
/**
 * Properties for defining a `CfnTargetGroup`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html
 */
export interface CfnTargetGroupProps {
    /**
     * The target group configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html#cfn-vpclattice-targetgroup-config
     */
    readonly config?: cdk.IResolvable | CfnTargetGroup.TargetGroupConfigProperty;
    /**
     * The name of the target group.
     *
     * The name must be unique within the account. The valid characters are a-z, 0-9, and hyphens (-). You can't use a hyphen as the first or last character, or immediately after another hyphen.
     *
     * If you don't specify a name, CloudFormation generates one. However, if you specify a name, and later want to replace the resource, you must specify a new name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html#cfn-vpclattice-targetgroup-name
     */
    readonly name?: string;
    /**
     * The tags for the target group.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html#cfn-vpclattice-targetgroup-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * Describes a target.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html#cfn-vpclattice-targetgroup-targets
     */
    readonly targets?: Array<cdk.IResolvable | CfnTargetGroup.TargetProperty> | cdk.IResolvable;
    /**
     * The type of target group.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-targetgroup.html#cfn-vpclattice-targetgroup-type
     */
    readonly type: string;
}
/**
 * A reference to a TargetGroup resource.
 *
 * @struct
 * @stability external
 */
export interface TargetGroupReference {
    /**
     * The Arn of the TargetGroup resource.
     */
    readonly targetGroupArn: string;
}
/**
 * Indicates that this resource can be referenced as a ResourceConfiguration.
 *
 * @stability experimental
 */
export interface IResourceConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a ResourceConfiguration resource.
     */
    readonly resourceConfigurationRef: ResourceConfigurationReference;
}
/**
 * Creates a resource configuration.
 *
 * A resource configuration defines a specific resource. You can associate a resource configuration with a service network or a VPC endpoint.
 *
 * @cloudformationResource AWS::VpcLattice::ResourceConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html
 */
export declare class CfnResourceConfiguration extends cdk.CfnResource implements cdk.IInspectable, IResourceConfigurationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnResourceConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnResourceConfiguration;
    /**
     * The Amazon Resource Name (ARN) of the resource configuration.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the resource configuration.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * Specifies whether the resource configuration can be associated with a sharable service network.
     */
    allowAssociationToSharableServiceNetwork?: boolean | cdk.IResolvable;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The name of the resource configuration.
     */
    name: string;
    /**
     * (SINGLE, GROUP, CHILD) The TCP port ranges that a consumer can use to access a resource configuration (for example: 1-65535).
     */
    portRanges?: Array<string>;
    /**
     * (SINGLE, GROUP) The protocol accepted by the resource configuration.
     */
    protocolType?: string;
    /**
     * The auth type for the resource configuration.
     */
    resourceConfigurationAuthType?: string;
    /**
     * Identifies the resource configuration in one of the following ways:.
     */
    resourceConfigurationDefinition?: cdk.IResolvable | CfnResourceConfiguration.ResourceConfigurationDefinitionProperty;
    /**
     * The ID of the group resource configuration.
     */
    resourceConfigurationGroupId?: string;
    /**
     * The type of resource configuration. A resource configuration can be one of the following types:.
     */
    resourceConfigurationType: string;
    /**
     * The ID of the resource gateway.
     */
    resourceGatewayId?: string;
    /**
     * The tags for the resource configuration.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnResourceConfigurationProps);
    get resourceConfigurationRef(): ResourceConfigurationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnResourceConfiguration {
    /**
     * Identifies the resource configuration in one of the following ways:.
     *
     * - *Amazon Resource Name (ARN)* - Supported resource-types that are provisioned by AWS services, such as RDS databases, can be identified by their ARN.
     * - *Domain name* - Any domain name that is publicly resolvable.
     * - *IP address* - For IPv4 and IPv6, only IP addresses in the VPC are supported.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-resourceconfigurationdefinition.html
     */
    interface ResourceConfigurationDefinitionProperty {
        /**
         * The Amazon Resource Name (ARN) of the resource configuration.
         *
         * For the ARN syntax and format, see [ARN format](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html#arns-syntax) in the *AWS Identity and Access Management user guide* .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-resourceconfigurationdefinition.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationdefinition-arnresource
         */
        readonly arnResource?: string;
        /**
         * The DNS name of the resource configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-resourceconfigurationdefinition.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationdefinition-dnsresource
         */
        readonly dnsResource?: CfnResourceConfiguration.DnsResourceProperty | cdk.IResolvable;
        /**
         * The IP address of the resource configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-resourceconfigurationdefinition.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationdefinition-ipresource
         */
        readonly ipResource?: string;
    }
    /**
     * The domain name of the resource configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-dnsresource.html
     */
    interface DnsResourceProperty {
        /**
         * The domain name of the resource configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-dnsresource.html#cfn-vpclattice-resourceconfiguration-dnsresource-domainname
         */
        readonly domainName: string;
        /**
         * The IP address type for the resource configuration.
         *
         * Dualstack is not currently supported.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-vpclattice-resourceconfiguration-dnsresource.html#cfn-vpclattice-resourceconfiguration-dnsresource-ipaddresstype
         */
        readonly ipAddressType: string;
    }
}
/**
 * Properties for defining a `CfnResourceConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html
 */
export interface CfnResourceConfigurationProps {
    /**
     * Specifies whether the resource configuration can be associated with a sharable service network.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-allowassociationtosharableservicenetwork
     */
    readonly allowAssociationToSharableServiceNetwork?: boolean | cdk.IResolvable;
    /**
     * The name of the resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-name
     */
    readonly name: string;
    /**
     * (SINGLE, GROUP, CHILD) The TCP port ranges that a consumer can use to access a resource configuration (for example: 1-65535).
     *
     * You can separate port ranges using commas (for example: 1,2,22-30).
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-portranges
     */
    readonly portRanges?: Array<string>;
    /**
     * (SINGLE, GROUP) The protocol accepted by the resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-protocoltype
     */
    readonly protocolType?: string;
    /**
     * The auth type for the resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationauthtype
     */
    readonly resourceConfigurationAuthType?: string;
    /**
     * Identifies the resource configuration in one of the following ways:.
     *
     * - *Amazon Resource Name (ARN)* - Supported resource-types that are provisioned by AWS services, such as RDS databases, can be identified by their ARN.
     * - *Domain name* - Any domain name that is publicly resolvable.
     * - *IP address* - For IPv4 and IPv6, only IP addresses in the VPC are supported.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationdefinition
     */
    readonly resourceConfigurationDefinition?: cdk.IResolvable | CfnResourceConfiguration.ResourceConfigurationDefinitionProperty;
    /**
     * The ID of the group resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationgroupid
     */
    readonly resourceConfigurationGroupId?: string;
    /**
     * The type of resource configuration. A resource configuration can be one of the following types:.
     *
     * - *SINGLE* - A single resource.
     * - *GROUP* - A group of resources. You must create a group resource configuration before you create a child resource configuration.
     * - *CHILD* - A single resource that is part of a group resource configuration.
     * - *ARN* - An AWS resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-resourceconfigurationtype
     */
    readonly resourceConfigurationType: string;
    /**
     * The ID of the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-resourcegatewayid
     */
    readonly resourceGatewayId?: string;
    /**
     * The tags for the resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourceconfiguration.html#cfn-vpclattice-resourceconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a ResourceConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface ResourceConfigurationReference {
    /**
     * The Arn of the ResourceConfiguration resource.
     */
    readonly resourceConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a ResourceGateway.
 *
 * @stability experimental
 */
export interface IResourceGatewayRef extends constructs.IConstruct {
    /**
     * A reference to a ResourceGateway resource.
     */
    readonly resourceGatewayRef: ResourceGatewayReference;
}
/**
 * A resource gateway is a point of ingress into the VPC where a resource resides.
 *
 * It spans multiple Availability Zones. For your resource to be accessible from all Availability Zones, you should create your resource gateways to span as many Availability Zones as possible. A VPC can have multiple resource gateways.
 *
 * @cloudformationResource AWS::VpcLattice::ResourceGateway
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html
 */
export declare class CfnResourceGateway extends cdk.CfnResource implements cdk.IInspectable, IResourceGatewayRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnResourceGateway from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnResourceGateway;
    /**
     * The Amazon Resource Name (ARN) of the resource gateway.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the resource gateway.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The type of IP address used by the resource gateway.
     */
    ipAddressType?: string;
    /**
     * The number of IPv4 addresses to allocate per ENI for the resource gateway.
     */
    ipv4AddressesPerEni?: number;
    /**
     * The name of the resource gateway.
     */
    name: string;
    /**
     * The IDs of the security groups applied to the resource gateway.
     */
    securityGroupIds?: Array<string>;
    /**
     * The IDs of the VPC subnets for the resource gateway.
     */
    subnetIds: Array<string>;
    /**
     * The tags for the resource gateway.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * The ID of the VPC for the resource gateway.
     */
    vpcIdentifier: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnResourceGatewayProps);
    get resourceGatewayRef(): ResourceGatewayReference;
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
 * Properties for defining a `CfnResourceGateway`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html
 */
export interface CfnResourceGatewayProps {
    /**
     * The type of IP address used by the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-ipaddresstype
     */
    readonly ipAddressType?: string;
    /**
     * The number of IPv4 addresses to allocate per ENI for the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-ipv4addressespereni
     */
    readonly ipv4AddressesPerEni?: number;
    /**
     * The name of the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-name
     */
    readonly name: string;
    /**
     * The IDs of the security groups applied to the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-securitygroupids
     */
    readonly securityGroupIds?: Array<string>;
    /**
     * The IDs of the VPC subnets for the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-subnetids
     */
    readonly subnetIds: Array<string>;
    /**
     * The tags for the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The ID of the VPC for the resource gateway.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-resourcegateway.html#cfn-vpclattice-resourcegateway-vpcidentifier
     */
    readonly vpcIdentifier: string;
}
/**
 * A reference to a ResourceGateway resource.
 *
 * @struct
 * @stability external
 */
export interface ResourceGatewayReference {
    /**
     * The Arn of the ResourceGateway resource.
     */
    readonly resourceGatewayArn: string;
}
/**
 * Indicates that this resource can be referenced as a ServiceNetworkResourceAssociation.
 *
 * @stability experimental
 */
export interface IServiceNetworkResourceAssociationRef extends constructs.IConstruct {
    /**
     * A reference to a ServiceNetworkResourceAssociation resource.
     */
    readonly serviceNetworkResourceAssociationRef: ServiceNetworkResourceAssociationReference;
}
/**
 * Associates the specified service network with the specified resource configuration.
 *
 * This allows the resource configuration to receive connections through the service network, including through a service network VPC endpoint.
 *
 * @cloudformationResource AWS::VpcLattice::ServiceNetworkResourceAssociation
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkresourceassociation.html
 */
export declare class CfnServiceNetworkResourceAssociation extends cdk.CfnResource implements cdk.IInspectable, IServiceNetworkResourceAssociationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnServiceNetworkResourceAssociation from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnServiceNetworkResourceAssociation;
    /**
     * The Amazon Resource Name (ARN) of the association.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The ID of the association between the service network and resource configuration.
     *
     * @cloudformationAttribute Id
     */
    readonly attrId: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The ID of the resource configuration associated with the service network.
     */
    resourceConfigurationId?: string;
    /**
     * The ID of the service network associated with the resource configuration.
     */
    serviceNetworkId?: string;
    /**
     * A key-value pair to associate with a resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnServiceNetworkResourceAssociationProps);
    get serviceNetworkResourceAssociationRef(): ServiceNetworkResourceAssociationReference;
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
 * Properties for defining a `CfnServiceNetworkResourceAssociation`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkresourceassociation.html
 */
export interface CfnServiceNetworkResourceAssociationProps {
    /**
     * The ID of the resource configuration associated with the service network.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkresourceassociation.html#cfn-vpclattice-servicenetworkresourceassociation-resourceconfigurationid
     */
    readonly resourceConfigurationId?: string;
    /**
     * The ID of the service network associated with the resource configuration.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkresourceassociation.html#cfn-vpclattice-servicenetworkresourceassociation-servicenetworkid
     */
    readonly serviceNetworkId?: string;
    /**
     * A key-value pair to associate with a resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-vpclattice-servicenetworkresourceassociation.html#cfn-vpclattice-servicenetworkresourceassociation-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a ServiceNetworkResourceAssociation resource.
 *
 * @struct
 * @stability external
 */
export interface ServiceNetworkResourceAssociationReference {
    /**
     * The Arn of the ServiceNetworkResourceAssociation resource.
     */
    readonly serviceNetworkResourceAssociationArn: string;
}
