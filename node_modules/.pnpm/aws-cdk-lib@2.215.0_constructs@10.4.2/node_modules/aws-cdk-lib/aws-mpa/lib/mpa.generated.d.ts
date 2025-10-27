import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a ApprovalTeam.
 *
 * @stability experimental
 */
export interface IApprovalTeamRef extends constructs.IConstruct {
    /**
     * A reference to a ApprovalTeam resource.
     */
    readonly approvalTeamRef: ApprovalTeamReference;
}
/**
 * Creates a new approval team.
 *
 * For more information, see [Approval team](https://docs.aws.amazon.com/mpa/latest/userguide/mpa-concepts.html) in the *Multi-party approval User Guide* .
 *
 * @cloudformationResource AWS::MPA::ApprovalTeam
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html
 */
export declare class CfnApprovalTeam extends cdk.CfnResource implements cdk.IInspectable, IApprovalTeamRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnApprovalTeam from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnApprovalTeam;
    /**
     * Amazon Resource Name (ARN) for the team.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Timestamp when the team was created.
     *
     * @cloudformationAttribute CreationTime
     */
    readonly attrCreationTime: string;
    /**
     * Timestamp when the team was last updated.
     *
     * @cloudformationAttribute LastUpdateTime
     */
    readonly attrLastUpdateTime: string;
    /**
     * Total number of approvers in the team.
     *
     * @cloudformationAttribute NumberOfApprovers
     */
    readonly attrNumberOfApprovers: number;
    /**
     * Status for the team. For more information, see [Team health](https://docs.aws.amazon.com/mpa/latest/userguide/mpa-health.html) in the *Multi-party approval User Guide* .
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * Status code for the team. For more information, see [Team health](https://docs.aws.amazon.com/mpa/latest/userguide/mpa-health.html) in the *Multi-party approval User Guide* .
     *
     * @cloudformationAttribute StatusCode
     */
    readonly attrStatusCode: string;
    /**
     * Message describing the status for the team.
     *
     * @cloudformationAttribute StatusMessage
     */
    readonly attrStatusMessage: string;
    /**
     * Timestamp when the team was last updated.
     *
     * @cloudformationAttribute UpdateSessionArn
     */
    readonly attrUpdateSessionArn: string;
    /**
     * Version ID for the team.
     *
     * @cloudformationAttribute VersionId
     */
    readonly attrVersionId: string;
    /**
     * Contains details for how an approval team grants approval.
     */
    approvalStrategy: CfnApprovalTeam.ApprovalStrategyProperty | cdk.IResolvable;
    /**
     * Contains details for an approver.
     */
    approvers: Array<CfnApprovalTeam.ApproverProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Description for the team.
     */
    description: string;
    /**
     * Name of the team.
     */
    name: string;
    /**
     * Contains details for a policy.
     */
    policies: Array<cdk.IResolvable | CfnApprovalTeam.PolicyProperty> | cdk.IResolvable;
    /**
     * Tags that you have added to the specified resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnApprovalTeamProps);
    get approvalTeamRef(): ApprovalTeamReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnApprovalTeam {
    /**
     * Strategy for how an approval team grants approval.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approvalstrategy.html
     */
    interface ApprovalStrategyProperty {
        /**
         * Minimum number of approvals (M) required for a total number of approvers (N).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approvalstrategy.html#cfn-mpa-approvalteam-approvalstrategy-mofn
         */
        readonly mofN: cdk.IResolvable | CfnApprovalTeam.MofNApprovalStrategyProperty;
    }
    /**
     * Strategy for how an approval team grants approval.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-mofnapprovalstrategy.html
     */
    interface MofNApprovalStrategyProperty {
        /**
         * Minimum number of approvals (M) required for a total number of approvers (N).
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-mofnapprovalstrategy.html#cfn-mpa-approvalteam-mofnapprovalstrategy-minapprovalsrequired
         */
        readonly minApprovalsRequired: number;
    }
    /**
     * Contains details for an approver.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html
     */
    interface ApproverProperty {
        /**
         * ID for the approver.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html#cfn-mpa-approvalteam-approver-approverid
         */
        readonly approverId?: string;
        /**
         * ID for the user.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html#cfn-mpa-approvalteam-approver-primaryidentityid
         */
        readonly primaryIdentityId: string;
        /**
         * Amazon Resource Name (ARN) for the identity source.
         *
         * The identity source manages the user authentication for approvers.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html#cfn-mpa-approvalteam-approver-primaryidentitysourcearn
         */
        readonly primaryIdentitySourceArn: string;
        /**
         * Status for the identity source.
         *
         * For example, if an approver has accepted a team invitation with a user authentication method managed by the identity source.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html#cfn-mpa-approvalteam-approver-primaryidentitystatus
         */
        readonly primaryIdentityStatus?: string;
        /**
         * Timestamp when the approver responded to an approval team invitation.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-approver.html#cfn-mpa-approvalteam-approver-responsetime
         */
        readonly responseTime?: string;
    }
    /**
     * Contains details for a policy.
     *
     * Policies define what operations a team that define the permissions for team resources.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-policy.html
     */
    interface PolicyProperty {
        /**
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-approvalteam-policy.html#cfn-mpa-approvalteam-policy-policyarn
         */
        readonly policyArn: string;
    }
}
/**
 * Properties for defining a `CfnApprovalTeam`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html
 */
export interface CfnApprovalTeamProps {
    /**
     * Contains details for how an approval team grants approval.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-approvalstrategy
     */
    readonly approvalStrategy: CfnApprovalTeam.ApprovalStrategyProperty | cdk.IResolvable;
    /**
     * Contains details for an approver.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-approvers
     */
    readonly approvers: Array<CfnApprovalTeam.ApproverProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * Description for the team.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-description
     */
    readonly description: string;
    /**
     * Name of the team.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-name
     */
    readonly name: string;
    /**
     * Contains details for a policy.
     *
     * Policies define what operations a team that define the permissions for team resources.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-policies
     */
    readonly policies: Array<cdk.IResolvable | CfnApprovalTeam.PolicyProperty> | cdk.IResolvable;
    /**
     * Tags that you have added to the specified resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-approvalteam.html#cfn-mpa-approvalteam-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a ApprovalTeam resource.
 *
 * @struct
 * @stability external
 */
export interface ApprovalTeamReference {
    /**
     * The Arn of the ApprovalTeam resource.
     */
    readonly approvalTeamArn: string;
}
/**
 * Indicates that this resource can be referenced as a IdentitySource.
 *
 * @stability experimental
 */
export interface IIdentitySourceRef extends constructs.IConstruct {
    /**
     * A reference to a IdentitySource resource.
     */
    readonly identitySourceRef: IdentitySourceReference;
}
/**
 * Creates a new identity source.
 *
 * For more information, see [Identity Source](https://docs.aws.amazon.com/mpa/latest/userguide/mpa-concepts.html) in the *Multi-party approval User Guide* .
 *
 * @cloudformationResource AWS::MPA::IdentitySource
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-identitysource.html
 */
export declare class CfnIdentitySource extends cdk.CfnResource implements cdk.IInspectable, IIdentitySourceRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnIdentitySource from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnIdentitySource;
    /**
     * Timestamp when the identity source was created.
     *
     * @cloudformationAttribute CreationTime
     */
    readonly attrCreationTime: string;
    /**
     * Amazon Resource Name (ARN) for the identity source.
     *
     * @cloudformationAttribute IdentitySourceArn
     */
    readonly attrIdentitySourceArn: string;
    /**
     * URL for the approval portal associated with the IAM Identity Center instance.
     *
     * @cloudformationAttribute IdentitySourceParameters.IamIdentityCenter.ApprovalPortalUrl
     */
    readonly attrIdentitySourceParametersIamIdentityCenterApprovalPortalUrl: string;
    /**
     * The type of resource that provided identities to the identity source. For example, an IAM Identity Center instance.
     *
     * @cloudformationAttribute IdentitySourceType
     */
    readonly attrIdentitySourceType: string;
    /**
     * Status for the identity source. For example, if the identity source is `ACTIVE` .
     *
     * @cloudformationAttribute Status
     */
    readonly attrStatus: string;
    /**
     * Status code of the identity source.
     *
     * @cloudformationAttribute StatusCode
     */
    readonly attrStatusCode: string;
    /**
     * Message describing the status for the identity source.
     *
     * @cloudformationAttribute StatusMessage
     */
    readonly attrStatusMessage: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * A `IdentitySourceParameters` object.
     */
    identitySourceParameters: CfnIdentitySource.IdentitySourceParametersProperty | cdk.IResolvable;
    /**
     * Tags that you have added to the specified resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnIdentitySourceProps);
    get identitySourceRef(): IdentitySourceReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnIdentitySource {
    /**
     * Contains details for the resource that provides identities to the identity source.
     *
     * For example, an IAM Identity Center instance.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-identitysourceparameters.html
     */
    interface IdentitySourceParametersProperty {
        /**
         * AWS IAM Identity Center credentials.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-identitysourceparameters.html#cfn-mpa-identitysource-identitysourceparameters-iamidentitycenter
         */
        readonly iamIdentityCenter: CfnIdentitySource.IamIdentityCenterProperty | cdk.IResolvable;
    }
    /**
     * AWS IAM Identity Center credentials.
     *
     * For more information see, [AWS IAM Identity Center](https://docs.aws.amazon.com/identity-center/) .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-iamidentitycenter.html
     */
    interface IamIdentityCenterProperty {
        /**
         * URL for the approval portal associated with the IAM Identity Center instance.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-iamidentitycenter.html#cfn-mpa-identitysource-iamidentitycenter-approvalportalurl
         */
        readonly approvalPortalUrl?: string;
        /**
         * Amazon Resource Name (ARN) for the IAM Identity Center instance.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-iamidentitycenter.html#cfn-mpa-identitysource-iamidentitycenter-instancearn
         */
        readonly instanceArn: string;
        /**
         * AWS Region where the IAM Identity Center instance is located.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-mpa-identitysource-iamidentitycenter.html#cfn-mpa-identitysource-iamidentitycenter-region
         */
        readonly region: string;
    }
}
/**
 * Properties for defining a `CfnIdentitySource`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-identitysource.html
 */
export interface CfnIdentitySourceProps {
    /**
     * A `IdentitySourceParameters` object.
     *
     * Contains details for the resource that provides identities to the identity source. For example, an IAM Identity Center instance.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-identitysource.html#cfn-mpa-identitysource-identitysourceparameters
     */
    readonly identitySourceParameters: CfnIdentitySource.IdentitySourceParametersProperty | cdk.IResolvable;
    /**
     * Tags that you have added to the specified resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-mpa-identitysource.html#cfn-mpa-identitysource-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a IdentitySource resource.
 *
 * @struct
 * @stability external
 */
export interface IdentitySourceReference {
    /**
     * The IdentitySourceArn of the IdentitySource resource.
     */
    readonly identitySourceArn: string;
}
