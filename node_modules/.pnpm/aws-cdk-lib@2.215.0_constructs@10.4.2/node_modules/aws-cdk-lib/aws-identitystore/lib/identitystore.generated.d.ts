import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a Group.
 *
 * @stability experimental
 */
export interface IGroupRef extends constructs.IConstruct {
    /**
     * A reference to a Group resource.
     */
    readonly groupRef: GroupReference;
}
/**
 * A group object, which contains a specified group’s metadata and attributes.
 *
 * @cloudformationResource AWS::IdentityStore::Group
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-group.html
 */
export declare class CfnGroup extends cdk.CfnResource implements cdk.IInspectable, IGroupRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnGroup from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnGroup;
    /**
     * The identifier of the newly created group in the identity store.
     *
     * @cloudformationAttribute GroupId
     */
    readonly attrGroupId: string;
    /**
     * A string containing the description of the group.
     */
    description?: string;
    /**
     * The display name value for the group.
     */
    displayName: string;
    /**
     * The globally unique identifier for the identity store.
     */
    identityStoreId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnGroupProps);
    get groupRef(): GroupReference;
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
 * Properties for defining a `CfnGroup`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-group.html
 */
export interface CfnGroupProps {
    /**
     * A string containing the description of the group.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-group.html#cfn-identitystore-group-description
     */
    readonly description?: string;
    /**
     * The display name value for the group.
     *
     * The length limit is 1,024 characters. This value can consist of letters, accented characters, symbols, numbers, punctuation, tab, new line, carriage return, space, and nonbreaking space in this attribute. This value is specified at the time the group is created and stored as an attribute of the group object in the identity store.
     *
     * Prefix search supports a maximum of 1,000 characters for the string.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-group.html#cfn-identitystore-group-displayname
     */
    readonly displayName: string;
    /**
     * The globally unique identifier for the identity store.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-group.html#cfn-identitystore-group-identitystoreid
     */
    readonly identityStoreId: string;
}
/**
 * A reference to a Group resource.
 *
 * @struct
 * @stability external
 */
export interface GroupReference {
    /**
     * The GroupId of the Group resource.
     */
    readonly groupId: string;
    /**
     * The IdentityStoreId of the Group resource.
     */
    readonly identityStoreId: string;
}
/**
 * Indicates that this resource can be referenced as a GroupMembership.
 *
 * @stability experimental
 */
export interface IGroupMembershipRef extends constructs.IConstruct {
    /**
     * A reference to a GroupMembership resource.
     */
    readonly groupMembershipRef: GroupMembershipReference;
}
/**
 * Creates a relationship between a member and a group.
 *
 * The following identifiers must be specified: `GroupId` , `IdentityStoreId` , and `MemberId` .
 *
 * @cloudformationResource AWS::IdentityStore::GroupMembership
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-groupmembership.html
 */
export declare class CfnGroupMembership extends cdk.CfnResource implements cdk.IInspectable, IGroupMembershipRef {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnGroupMembership from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnGroupMembership;
    /**
     * The identifier for a `GroupMembership` in the identity store.
     *
     * @cloudformationAttribute MembershipId
     */
    readonly attrMembershipId: string;
    /**
     * The identifier for a group in the identity store.
     */
    groupId: string;
    /**
     * The globally unique identifier for the identity store.
     */
    identityStoreId: string;
    /**
     * An object containing the identifier of a group member.
     */
    memberId: cdk.IResolvable | CfnGroupMembership.MemberIdProperty;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnGroupMembershipProps);
    get groupMembershipRef(): GroupMembershipReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnGroupMembership {
    /**
     * An object that contains the identifier of a group member.
     *
     * Setting the `UserID` field to the specific identifier for a user indicates that the user is a member of the group.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-identitystore-groupmembership-memberid.html
     */
    interface MemberIdProperty {
        /**
         * An object containing the identifiers of resources that can be members.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-identitystore-groupmembership-memberid.html#cfn-identitystore-groupmembership-memberid-userid
         */
        readonly userId: string;
    }
}
/**
 * Properties for defining a `CfnGroupMembership`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-groupmembership.html
 */
export interface CfnGroupMembershipProps {
    /**
     * The identifier for a group in the identity store.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-groupmembership.html#cfn-identitystore-groupmembership-groupid
     */
    readonly groupId: string;
    /**
     * The globally unique identifier for the identity store.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-groupmembership.html#cfn-identitystore-groupmembership-identitystoreid
     */
    readonly identityStoreId: string;
    /**
     * An object containing the identifier of a group member.
     *
     * Setting the `MemberId` 's `UserId` field to a specific User's ID indicates that user is a member of the group.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-identitystore-groupmembership.html#cfn-identitystore-groupmembership-memberid
     */
    readonly memberId: cdk.IResolvable | CfnGroupMembership.MemberIdProperty;
}
/**
 * A reference to a GroupMembership resource.
 *
 * @struct
 * @stability external
 */
export interface GroupMembershipReference {
    /**
     * The MembershipId of the GroupMembership resource.
     */
    readonly membershipId: string;
    /**
     * The IdentityStoreId of the GroupMembership resource.
     */
    readonly identityStoreId: string;
}
