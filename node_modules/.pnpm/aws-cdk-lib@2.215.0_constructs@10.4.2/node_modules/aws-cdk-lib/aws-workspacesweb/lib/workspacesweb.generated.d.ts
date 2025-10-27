import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a BrowserSettings.
 *
 * @stability experimental
 */
export interface IBrowserSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a BrowserSettings resource.
     */
    readonly browserSettingsRef: BrowserSettingsReference;
}
/**
 * This resource specifies browser settings that can be associated with a web portal.
 *
 * Once associated with a web portal, browser settings control how the browser will behave once a user starts a streaming session for the web portal.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::BrowserSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html
 */
export declare class CfnBrowserSettings extends cdk.CfnResource implements cdk.IInspectable, IBrowserSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnBrowserSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnBrowserSettings;
    /**
     * A list of web portal ARNs that the browser settings resource is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The ARN of the browser settings.
     *
     * @cloudformationAttribute BrowserSettingsArn
     */
    readonly attrBrowserSettingsArn: string;
    /**
     * Additional encryption context of the browser settings.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * A JSON string containing Chrome Enterprise policies that will be applied to all streaming sessions.
     */
    browserPolicy?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The custom managed key of the browser settings.
     */
    customerManagedKey?: string;
    /**
     * The tags to add to the browser settings resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnBrowserSettingsProps);
    get browserSettingsRef(): BrowserSettingsReference;
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
 * Properties for defining a `CfnBrowserSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html
 */
export interface CfnBrowserSettingsProps {
    /**
     * Additional encryption context of the browser settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html#cfn-workspacesweb-browsersettings-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * A JSON string containing Chrome Enterprise policies that will be applied to all streaming sessions.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html#cfn-workspacesweb-browsersettings-browserpolicy
     */
    readonly browserPolicy?: string;
    /**
     * The custom managed key of the browser settings.
     *
     * *Pattern* : `^arn:[\w+=\/,.@-]+:kms:[a-zA-Z0-9\-]*:[a-zA-Z0-9]{1,12}:key\/[a-zA-Z0-9-]+$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html#cfn-workspacesweb-browsersettings-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * The tags to add to the browser settings resource.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-browsersettings.html#cfn-workspacesweb-browsersettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a BrowserSettings resource.
 *
 * @struct
 * @stability external
 */
export interface BrowserSettingsReference {
    /**
     * The BrowserSettingsArn of the BrowserSettings resource.
     */
    readonly browserSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a IdentityProvider.
 *
 * @stability experimental
 */
export interface IIdentityProviderRef extends constructs.IConstruct {
    /**
     * A reference to a IdentityProvider resource.
     */
    readonly identityProviderRef: IdentityProviderReference;
}
/**
 * This resource specifies an identity provider that is then associated with a web portal.
 *
 * This resource is not required if your portal's `AuthenticationType` is IAM Identity Center.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::IdentityProvider
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html
 */
export declare class CfnIdentityProvider extends cdk.CfnResource implements cdk.IInspectable, IIdentityProviderRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnIdentityProvider from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnIdentityProvider;
    /**
     * The ARN of the identity provider.
     *
     * @cloudformationAttribute IdentityProviderArn
     */
    readonly attrIdentityProviderArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The identity provider details. The following list describes the provider detail keys for each identity provider type.
     */
    identityProviderDetails: cdk.IResolvable | Record<string, string>;
    /**
     * The identity provider name.
     */
    identityProviderName: string;
    /**
     * The identity provider type.
     */
    identityProviderType: string;
    /**
     * The ARN of the identity provider.
     */
    portalArn?: string;
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnIdentityProviderProps);
    get identityProviderRef(): IdentityProviderReference;
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
 * Properties for defining a `CfnIdentityProvider`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html
 */
export interface CfnIdentityProviderProps {
    /**
     * The identity provider details. The following list describes the provider detail keys for each identity provider type.
     *
     * - For Google and Login with Amazon:
     *
     * - `client_id`
     * - `client_secret`
     * - `authorize_scopes`
     * - For Facebook:
     *
     * - `client_id`
     * - `client_secret`
     * - `authorize_scopes`
     * - `api_version`
     * - For Sign in with Apple:
     *
     * - `client_id`
     * - `team_id`
     * - `key_id`
     * - `private_key`
     * - `authorize_scopes`
     * - For OIDC providers:
     *
     * - `client_id`
     * - `client_secret`
     * - `attributes_request_method`
     * - `oidc_issuer`
     * - `authorize_scopes`
     * - `authorize_url` *if not available from discovery URL specified by oidc_issuer key*
     * - `token_url` *if not available from discovery URL specified by oidc_issuer key*
     * - `attributes_url` *if not available from discovery URL specified by oidc_issuer key*
     * - `jwks_uri` *if not available from discovery URL specified by oidc_issuer key*
     * - For SAML providers:
     *
     * - `MetadataFile` OR `MetadataURL`
     * - `IDPSignout` (boolean) *optional*
     * - `IDPInit` (boolean) *optional*
     * - `RequestSigningAlgorithm` (string) *optional* - Only accepts `rsa-sha256`
     * - `EncryptedResponses` (boolean) *optional*
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html#cfn-workspacesweb-identityprovider-identityproviderdetails
     */
    readonly identityProviderDetails: cdk.IResolvable | Record<string, string>;
    /**
     * The identity provider name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html#cfn-workspacesweb-identityprovider-identityprovidername
     */
    readonly identityProviderName: string;
    /**
     * The identity provider type.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html#cfn-workspacesweb-identityprovider-identityprovidertype
     */
    readonly identityProviderType: string;
    /**
     * The ARN of the identity provider.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html#cfn-workspacesweb-identityprovider-portalarn
     */
    readonly portalArn?: string;
    /**
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-identityprovider.html#cfn-workspacesweb-identityprovider-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a IdentityProvider resource.
 *
 * @struct
 * @stability external
 */
export interface IdentityProviderReference {
    /**
     * The IdentityProviderArn of the IdentityProvider resource.
     */
    readonly identityProviderArn: string;
}
/**
 * Indicates that this resource can be referenced as a IpAccessSettings.
 *
 * @stability experimental
 */
export interface IIpAccessSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a IpAccessSettings resource.
     */
    readonly ipAccessSettingsRef: IpAccessSettingsReference;
}
/**
 * This resource specifies IP access settings that can be associated with a web portal.
 *
 * For more information, see [Set up IP access controls (optional)](https://docs.aws.amazon.com/workspaces-web/latest/adminguide/ip-access-controls.html) .
 *
 * @cloudformationResource AWS::WorkSpacesWeb::IpAccessSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html
 */
export declare class CfnIpAccessSettings extends cdk.CfnResource implements cdk.IInspectable, IIpAccessSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnIpAccessSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnIpAccessSettings;
    /**
     * A list of web portal ARNs that this IP access settings resource is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The creation date timestamp of the IP access settings.
     *
     * @cloudformationAttribute CreationDate
     */
    readonly attrCreationDate: string;
    /**
     * The ARN of the IP access settings resource.
     *
     * @cloudformationAttribute IpAccessSettingsArn
     */
    readonly attrIpAccessSettingsArn: string;
    /**
     * Additional encryption context of the IP access settings.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The custom managed key of the IP access settings.
     */
    customerManagedKey?: string;
    /**
     * The description of the IP access settings.
     */
    description?: string;
    /**
     * The display name of the IP access settings.
     */
    displayName?: string;
    /**
     * The IP rules of the IP access settings.
     */
    ipRules: Array<CfnIpAccessSettings.IpRuleProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * The tags to add to the IP access settings resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnIpAccessSettingsProps);
    get ipAccessSettingsRef(): IpAccessSettingsReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnIpAccessSettings {
    /**
     * The IP rules of the IP access settings.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-ipaccesssettings-iprule.html
     */
    interface IpRuleProperty {
        /**
         * The description of the IP rule.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-ipaccesssettings-iprule.html#cfn-workspacesweb-ipaccesssettings-iprule-description
         */
        readonly description?: string;
        /**
         * The IP range of the IP rule.
         *
         * This can either be a single IP address or a range using CIDR notation.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-ipaccesssettings-iprule.html#cfn-workspacesweb-ipaccesssettings-iprule-iprange
         */
        readonly ipRange: string;
    }
}
/**
 * Properties for defining a `CfnIpAccessSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html
 */
export interface CfnIpAccessSettingsProps {
    /**
     * Additional encryption context of the IP access settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The custom managed key of the IP access settings.
     *
     * *Pattern* : `^arn:[\w+=\/,.@-]+:kms:[a-zA-Z0-9\-]*:[a-zA-Z0-9]{1,12}:key\/[a-zA-Z0-9-]+$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * The description of the IP access settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-description
     */
    readonly description?: string;
    /**
     * The display name of the IP access settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-displayname
     */
    readonly displayName?: string;
    /**
     * The IP rules of the IP access settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-iprules
     */
    readonly ipRules: Array<CfnIpAccessSettings.IpRuleProperty | cdk.IResolvable> | cdk.IResolvable;
    /**
     * The tags to add to the IP access settings resource.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-ipaccesssettings.html#cfn-workspacesweb-ipaccesssettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a IpAccessSettings resource.
 *
 * @struct
 * @stability external
 */
export interface IpAccessSettingsReference {
    /**
     * The IpAccessSettingsArn of the IpAccessSettings resource.
     */
    readonly ipAccessSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a NetworkSettings.
 *
 * @stability experimental
 */
export interface INetworkSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a NetworkSettings resource.
     */
    readonly networkSettingsRef: NetworkSettingsReference;
}
/**
 * This resource specifies network settings that can be associated with a web portal.
 *
 * Once associated with a web portal, network settings define how streaming instances will connect with your specified VPC.
 *
 * The VPC must have default tenancy. VPCs with dedicated tenancy are not supported.
 *
 * For availability consideration, you must have at least two subnets created in two different Availability Zones. WorkSpaces Secure Browser is available in a subset of the Availability Zones for each supported Region. For more information, see [Supported Availability Zones](https://docs.aws.amazon.com/workspaces-web/latest/adminguide/availability-zones.html) .
 *
 * @cloudformationResource AWS::WorkSpacesWeb::NetworkSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html
 */
export declare class CfnNetworkSettings extends cdk.CfnResource implements cdk.IInspectable, INetworkSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnNetworkSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnNetworkSettings;
    /**
     * A list of web portal ARNs that this network settings is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The ARN of the network settings.
     *
     * @cloudformationAttribute NetworkSettingsArn
     */
    readonly attrNetworkSettingsArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * One or more security groups used to control access from streaming instances to your VPC.
     */
    securityGroupIds: Array<string>;
    /**
     * The subnets in which network interfaces are created to connect streaming instances to your VPC.
     */
    subnetIds: Array<string>;
    /**
     * The tags to add to the network settings resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * The VPC that streaming instances will connect to.
     */
    vpcId: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnNetworkSettingsProps);
    get networkSettingsRef(): NetworkSettingsReference;
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
 * Properties for defining a `CfnNetworkSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html
 */
export interface CfnNetworkSettingsProps {
    /**
     * One or more security groups used to control access from streaming instances to your VPC.
     *
     * *Pattern* : `^[\w+\-]+$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html#cfn-workspacesweb-networksettings-securitygroupids
     */
    readonly securityGroupIds: Array<string>;
    /**
     * The subnets in which network interfaces are created to connect streaming instances to your VPC.
     *
     * At least two of these subnets must be in different availability zones.
     *
     * *Pattern* : `^subnet-([0-9a-f]{8}|[0-9a-f]{17})$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html#cfn-workspacesweb-networksettings-subnetids
     */
    readonly subnetIds: Array<string>;
    /**
     * The tags to add to the network settings resource.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html#cfn-workspacesweb-networksettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The VPC that streaming instances will connect to.
     *
     * *Pattern* : `^vpc-[0-9a-z]*$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-networksettings.html#cfn-workspacesweb-networksettings-vpcid
     */
    readonly vpcId: string;
}
/**
 * A reference to a NetworkSettings resource.
 *
 * @struct
 * @stability external
 */
export interface NetworkSettingsReference {
    /**
     * The NetworkSettingsArn of the NetworkSettings resource.
     */
    readonly networkSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a Portal.
 *
 * @stability experimental
 */
export interface IPortalRef extends constructs.IConstruct {
    /**
     * A reference to a Portal resource.
     */
    readonly portalRef: PortalReference;
}
/**
 * This resource specifies a web portal, which users use to start browsing sessions.
 *
 * A `Standard` web portal can't start browsing sessions unless you have at defined and associated an `IdentityProvider` and `NetworkSettings` resource. An `IAM Identity Center` web portal does not require an `IdentityProvider` resource.
 *
 * For more information about web portals, see [What is Amazon WorkSpaces Secure Browser?](https://docs.aws.amazon.com/workspaces-web/latest/adminguide/what-is-workspaces-web.html.html) .
 *
 * @cloudformationResource AWS::WorkSpacesWeb::Portal
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html
 */
export declare class CfnPortal extends cdk.CfnResource implements cdk.IInspectable, IPortalRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnPortal from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnPortal;
    /**
     * The browser that users see when using a streaming session.
     *
     * @cloudformationAttribute BrowserType
     */
    readonly attrBrowserType: string;
    /**
     * The creation date of the web portal.
     *
     * @cloudformationAttribute CreationDate
     */
    readonly attrCreationDate: string;
    /**
     * The ARN of the web portal.
     *
     * @cloudformationAttribute PortalArn
     */
    readonly attrPortalArn: string;
    /**
     * The endpoint URL of the web portal that users access in order to start streaming sessions.
     *
     * @cloudformationAttribute PortalEndpoint
     */
    readonly attrPortalEndpoint: string;
    /**
     * The status of the web portal.
     *
     * @cloudformationAttribute PortalStatus
     */
    readonly attrPortalStatus: string;
    /**
     * The renderer that is used in streaming sessions.
     *
     * @cloudformationAttribute RendererType
     */
    readonly attrRendererType: string;
    /**
     * The SAML metadata of the service provider.
     *
     * @cloudformationAttribute ServiceProviderSamlMetadata
     */
    readonly attrServiceProviderSamlMetadata: string;
    /**
     * A message that explains why the web portal is in its current status.
     *
     * @cloudformationAttribute StatusReason
     */
    readonly attrStatusReason: string;
    /**
     * The additional encryption context of the portal.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The type of authentication integration points used when signing into the web portal. Defaults to `Standard` .
     */
    authenticationType?: string;
    /**
     * The ARN of the browser settings that is associated with this web portal.
     */
    browserSettingsArn?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The customer managed key of the web portal.
     */
    customerManagedKey?: string;
    /**
     * The ARN of the data protection settings.
     */
    dataProtectionSettingsArn?: string;
    /**
     * The name of the web portal.
     */
    displayName?: string;
    /**
     * The type and resources of the underlying instance.
     */
    instanceType?: string;
    /**
     * The ARN of the IP access settings that is associated with the web portal.
     */
    ipAccessSettingsArn?: string;
    /**
     * The maximum number of concurrent sessions for the portal.
     */
    maxConcurrentSessions?: number;
    /**
     * The ARN of the network settings that is associated with the web portal.
     */
    networkSettingsArn?: string;
    /**
     * The ARN of the session logger that is assocaited with the portal.
     */
    sessionLoggerArn?: string;
    /**
     * The tags to add to the web portal.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * The ARN of the trust store that is associated with the web portal.
     */
    trustStoreArn?: string;
    /**
     * The ARN of the user access logging settings that is associated with the web portal.
     */
    userAccessLoggingSettingsArn?: string;
    /**
     * The ARN of the user settings that is associated with the web portal.
     */
    userSettingsArn?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnPortalProps);
    get portalRef(): PortalReference;
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
 * Properties for defining a `CfnPortal`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html
 */
export interface CfnPortalProps {
    /**
     * The additional encryption context of the portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The type of authentication integration points used when signing into the web portal. Defaults to `Standard` .
     *
     * `Standard` web portals are authenticated directly through your identity provider (IdP). User and group access to your web portal is controlled through your IdP. You need to include an IdP resource in your template to integrate your IdP with your web portal. Completing the configuration for your IdP requires exchanging WorkSpaces Secure Browser’s SP metadata with your IdP’s IdP metadata. If your IdP requires the SP metadata first before returning the IdP metadata, you should follow these steps:
     *
     * 1. Create and deploy a CloudFormation template with a `Standard` portal with no `IdentityProvider` resource.
     *
     * 2. Retrieve the SP metadata using `Fn:GetAtt` , the WorkSpaces Secure Browser console, or by the calling the `GetPortalServiceProviderMetadata` API.
     *
     * 3. Submit the data to your IdP.
     *
     * 4. Add an `IdentityProvider` resource to your CloudFormation template.
     *
     * `IAM Identity Center` web portals are authenticated through AWS IAM Identity Center . They provide additional features, such as IdP-initiated authentication. Identity sources (including external identity provider integration) and other identity provider information must be configured in IAM Identity Center . User and group assignment must be done through the WorkSpaces Secure Browser console. These cannot be configured in CloudFormation.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-authenticationtype
     */
    readonly authenticationType?: string;
    /**
     * The ARN of the browser settings that is associated with this web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-browsersettingsarn
     */
    readonly browserSettingsArn?: string;
    /**
     * The customer managed key of the web portal.
     *
     * *Pattern* : `^arn:[\w+=\/,.@-]+:kms:[a-zA-Z0-9\-]*:[a-zA-Z0-9]{1,12}:key\/[a-zA-Z0-9-]+$`
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * The ARN of the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-dataprotectionsettingsarn
     */
    readonly dataProtectionSettingsArn?: string;
    /**
     * The name of the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-displayname
     */
    readonly displayName?: string;
    /**
     * The type and resources of the underlying instance.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-instancetype
     */
    readonly instanceType?: string;
    /**
     * The ARN of the IP access settings that is associated with the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-ipaccesssettingsarn
     */
    readonly ipAccessSettingsArn?: string;
    /**
     * The maximum number of concurrent sessions for the portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-maxconcurrentsessions
     */
    readonly maxConcurrentSessions?: number;
    /**
     * The ARN of the network settings that is associated with the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-networksettingsarn
     */
    readonly networkSettingsArn?: string;
    /**
     * The ARN of the session logger that is assocaited with the portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-sessionloggerarn
     */
    readonly sessionLoggerArn?: string;
    /**
     * The tags to add to the web portal.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The ARN of the trust store that is associated with the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-truststorearn
     */
    readonly trustStoreArn?: string;
    /**
     * The ARN of the user access logging settings that is associated with the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-useraccessloggingsettingsarn
     */
    readonly userAccessLoggingSettingsArn?: string;
    /**
     * The ARN of the user settings that is associated with the web portal.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-portal.html#cfn-workspacesweb-portal-usersettingsarn
     */
    readonly userSettingsArn?: string;
}
/**
 * A reference to a Portal resource.
 *
 * @struct
 * @stability external
 */
export interface PortalReference {
    /**
     * The PortalArn of the Portal resource.
     */
    readonly portalArn: string;
}
/**
 * Indicates that this resource can be referenced as a TrustStore.
 *
 * @stability experimental
 */
export interface ITrustStoreRef extends constructs.IConstruct {
    /**
     * A reference to a TrustStore resource.
     */
    readonly trustStoreRef: TrustStoreReference;
}
/**
 * This resource specifies a trust store that can be associated with a web portal.
 *
 * A trust store contains certificate authority (CA) certificates. Once associated with a web portal, the browser in a streaming session will recognize certificates that have been issued using any of the CAs in the trust store. If your organization has internal websites that use certificates issued by private CAs, you should add the private CA certificate to the trust store.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::TrustStore
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-truststore.html
 */
export declare class CfnTrustStore extends cdk.CfnResource implements cdk.IInspectable, ITrustStoreRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnTrustStore from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnTrustStore;
    /**
     * A list of web portal ARNs that this trust store is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The ARN of the trust store.
     *
     * @cloudformationAttribute TrustStoreArn
     */
    readonly attrTrustStoreArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * A list of CA certificates to be added to the trust store.
     */
    certificateList: Array<string>;
    /**
     * The tags to add to the trust store.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnTrustStoreProps);
    get trustStoreRef(): TrustStoreReference;
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
 * Properties for defining a `CfnTrustStore`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-truststore.html
 */
export interface CfnTrustStoreProps {
    /**
     * A list of CA certificates to be added to the trust store.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-truststore.html#cfn-workspacesweb-truststore-certificatelist
     */
    readonly certificateList: Array<string>;
    /**
     * The tags to add to the trust store.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-truststore.html#cfn-workspacesweb-truststore-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a TrustStore resource.
 *
 * @struct
 * @stability external
 */
export interface TrustStoreReference {
    /**
     * The TrustStoreArn of the TrustStore resource.
     */
    readonly trustStoreArn: string;
}
/**
 * Indicates that this resource can be referenced as a UserAccessLoggingSettings.
 *
 * @stability experimental
 */
export interface IUserAccessLoggingSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a UserAccessLoggingSettings resource.
     */
    readonly userAccessLoggingSettingsRef: UserAccessLoggingSettingsReference;
}
/**
 * This resource specifies user access logging settings that can be associated with a web portal.
 *
 * In order to receive logs from WorkSpaces Secure Browser, you must have an Amazon Kinesis Data Stream that starts with "amazon-workspaces-web-*". Your Amazon Kinesis data stream must either have server-side encryption turned off, or must use AWS managed keys for server-side encryption.
 *
 * For more information about setting server-side encryption in Amazon Kinesis , see [How Do I Get Started with Server-Side Encryption?](https://docs.aws.amazon.com/streams/latest/dev/getting-started-with-sse.html) .
 *
 * For more information about setting up user access logging, see [Set up user access logging](https://docs.aws.amazon.com/workspaces-web/latest/adminguide/user-logging.html) .
 *
 * @cloudformationResource AWS::WorkSpacesWeb::UserAccessLoggingSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-useraccessloggingsettings.html
 */
export declare class CfnUserAccessLoggingSettings extends cdk.CfnResource implements cdk.IInspectable, IUserAccessLoggingSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnUserAccessLoggingSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnUserAccessLoggingSettings;
    /**
     * A list of web portal ARNs that this user access logging settings is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The ARN of the user access logging settings.
     *
     * @cloudformationAttribute UserAccessLoggingSettingsArn
     */
    readonly attrUserAccessLoggingSettingsArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The ARN of the Kinesis stream.
     */
    kinesisStreamArn: string;
    /**
     * The tags to add to the user access logging settings resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnUserAccessLoggingSettingsProps);
    get userAccessLoggingSettingsRef(): UserAccessLoggingSettingsReference;
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
 * Properties for defining a `CfnUserAccessLoggingSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-useraccessloggingsettings.html
 */
export interface CfnUserAccessLoggingSettingsProps {
    /**
     * The ARN of the Kinesis stream.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-useraccessloggingsettings.html#cfn-workspacesweb-useraccessloggingsettings-kinesisstreamarn
     */
    readonly kinesisStreamArn: string;
    /**
     * The tags to add to the user access logging settings resource.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-useraccessloggingsettings.html#cfn-workspacesweb-useraccessloggingsettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a UserAccessLoggingSettings resource.
 *
 * @struct
 * @stability external
 */
export interface UserAccessLoggingSettingsReference {
    /**
     * The UserAccessLoggingSettingsArn of the UserAccessLoggingSettings resource.
     */
    readonly userAccessLoggingSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a UserSettings.
 *
 * @stability experimental
 */
export interface IUserSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a UserSettings resource.
     */
    readonly userSettingsRef: UserSettingsReference;
}
/**
 * This resource specifies user settings that can be associated with a web portal.
 *
 * Once associated with a web portal, user settings control how users can transfer data between a streaming session and the their local devices.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::UserSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html
 */
export declare class CfnUserSettings extends cdk.CfnResource implements cdk.IInspectable, IUserSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnUserSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnUserSettings;
    /**
     * A list of web portal ARNs that this user settings resource is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The ARN of the user settings.
     *
     * @cloudformationAttribute UserSettingsArn
     */
    readonly attrUserSettingsArn: string;
    /**
     * The additional encryption context of the user settings.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The configuration that specifies which cookies should be synchronized from the end user's local browser to the remote browser.
     */
    cookieSynchronizationConfiguration?: CfnUserSettings.CookieSynchronizationConfigurationProperty | cdk.IResolvable;
    /**
     * Specifies whether the user can copy text from the streaming session to the local device.
     */
    copyAllowed: string;
    /**
     * The customer managed key used to encrypt sensitive information in the user settings.
     */
    customerManagedKey?: string;
    /**
     * Specifies whether the user can use deep links that open automatically when connecting to a session.
     */
    deepLinkAllowed?: string;
    /**
     * The amount of time that a streaming session remains active after users disconnect.
     */
    disconnectTimeoutInMinutes?: number;
    /**
     * Specifies whether the user can download files from the streaming session to the local device.
     */
    downloadAllowed: string;
    /**
     * The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the disconnect timeout interval begins.
     */
    idleDisconnectTimeoutInMinutes?: number;
    /**
     * Specifies whether the user can paste text from the local device to the streaming session.
     */
    pasteAllowed: string;
    /**
     * Specifies whether the user can print to the local device.
     */
    printAllowed: string;
    /**
     * The tags to add to the user settings resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * The configuration of the toolbar.
     */
    toolbarConfiguration?: cdk.IResolvable | CfnUserSettings.ToolbarConfigurationProperty;
    /**
     * Specifies whether the user can upload files from the local device to the streaming session.
     */
    uploadAllowed: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnUserSettingsProps);
    get userSettingsRef(): UserSettingsReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnUserSettings {
    /**
     * The configuration that specifies which cookies should be synchronized from the end user's local browser to the remote browser.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiesynchronizationconfiguration.html
     */
    interface CookieSynchronizationConfigurationProperty {
        /**
         * The list of cookie specifications that are allowed to be synchronized to the remote browser.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiesynchronizationconfiguration.html#cfn-workspacesweb-usersettings-cookiesynchronizationconfiguration-allowlist
         */
        readonly allowlist: Array<CfnUserSettings.CookieSpecificationProperty | cdk.IResolvable> | cdk.IResolvable;
        /**
         * The list of cookie specifications that are blocked from being synchronized to the remote browser.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiesynchronizationconfiguration.html#cfn-workspacesweb-usersettings-cookiesynchronizationconfiguration-blocklist
         */
        readonly blocklist?: Array<CfnUserSettings.CookieSpecificationProperty | cdk.IResolvable> | cdk.IResolvable;
    }
    /**
     * Specifies a single cookie or set of cookies in an end user's browser.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiespecification.html
     */
    interface CookieSpecificationProperty {
        /**
         * The domain of the cookie.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiespecification.html#cfn-workspacesweb-usersettings-cookiespecification-domain
         */
        readonly domain: string;
        /**
         * The name of the cookie.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiespecification.html#cfn-workspacesweb-usersettings-cookiespecification-name
         */
        readonly name?: string;
        /**
         * The path of the cookie.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-cookiespecification.html#cfn-workspacesweb-usersettings-cookiespecification-path
         */
        readonly path?: string;
    }
    /**
     * The configuration of the toolbar.
     *
     * This allows administrators to select the toolbar type and visual mode, set maximum display resolution for sessions, and choose which items are visible to end users during their sessions. If administrators do not modify these settings, end users retain control over their toolbar preferences.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-toolbarconfiguration.html
     */
    interface ToolbarConfigurationProperty {
        /**
         * The list of toolbar items to be hidden.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-toolbarconfiguration.html#cfn-workspacesweb-usersettings-toolbarconfiguration-hiddentoolbaritems
         */
        readonly hiddenToolbarItems?: Array<string>;
        /**
         * The maximum display resolution that is allowed for the session.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-toolbarconfiguration.html#cfn-workspacesweb-usersettings-toolbarconfiguration-maxdisplayresolution
         */
        readonly maxDisplayResolution?: string;
        /**
         * The type of toolbar displayed during the session.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-toolbarconfiguration.html#cfn-workspacesweb-usersettings-toolbarconfiguration-toolbartype
         */
        readonly toolbarType?: string;
        /**
         * The visual mode of the toolbar.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-usersettings-toolbarconfiguration.html#cfn-workspacesweb-usersettings-toolbarconfiguration-visualmode
         */
        readonly visualMode?: string;
    }
}
/**
 * Properties for defining a `CfnUserSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html
 */
export interface CfnUserSettingsProps {
    /**
     * The additional encryption context of the user settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The configuration that specifies which cookies should be synchronized from the end user's local browser to the remote browser.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-cookiesynchronizationconfiguration
     */
    readonly cookieSynchronizationConfiguration?: CfnUserSettings.CookieSynchronizationConfigurationProperty | cdk.IResolvable;
    /**
     * Specifies whether the user can copy text from the streaming session to the local device.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-copyallowed
     */
    readonly copyAllowed: string;
    /**
     * The customer managed key used to encrypt sensitive information in the user settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * Specifies whether the user can use deep links that open automatically when connecting to a session.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-deeplinkallowed
     */
    readonly deepLinkAllowed?: string;
    /**
     * The amount of time that a streaming session remains active after users disconnect.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-disconnecttimeoutinminutes
     */
    readonly disconnectTimeoutInMinutes?: number;
    /**
     * Specifies whether the user can download files from the streaming session to the local device.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-downloadallowed
     */
    readonly downloadAllowed: string;
    /**
     * The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the disconnect timeout interval begins.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-idledisconnecttimeoutinminutes
     */
    readonly idleDisconnectTimeoutInMinutes?: number;
    /**
     * Specifies whether the user can paste text from the local device to the streaming session.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-pasteallowed
     */
    readonly pasteAllowed: string;
    /**
     * Specifies whether the user can print to the local device.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-printallowed
     */
    readonly printAllowed: string;
    /**
     * The tags to add to the user settings resource.
     *
     * A tag is a key-value pair.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * The configuration of the toolbar.
     *
     * This allows administrators to select the toolbar type and visual mode, set maximum display resolution for sessions, and choose which items are visible to end users during their sessions. If administrators do not modify these settings, end users retain control over their toolbar preferences.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-toolbarconfiguration
     */
    readonly toolbarConfiguration?: cdk.IResolvable | CfnUserSettings.ToolbarConfigurationProperty;
    /**
     * Specifies whether the user can upload files from the local device to the streaming session.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-usersettings.html#cfn-workspacesweb-usersettings-uploadallowed
     */
    readonly uploadAllowed: string;
}
/**
 * A reference to a UserSettings resource.
 *
 * @struct
 * @stability external
 */
export interface UserSettingsReference {
    /**
     * The UserSettingsArn of the UserSettings resource.
     */
    readonly userSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a DataProtectionSettings.
 *
 * @stability experimental
 */
export interface IDataProtectionSettingsRef extends constructs.IConstruct {
    /**
     * A reference to a DataProtectionSettings resource.
     */
    readonly dataProtectionSettingsRef: DataProtectionSettingsReference;
}
/**
 * The data protection settings resource that can be associated with a web portal.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::DataProtectionSettings
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html
 */
export declare class CfnDataProtectionSettings extends cdk.CfnResource implements cdk.IInspectable, IDataProtectionSettingsRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnDataProtectionSettings from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnDataProtectionSettings;
    /**
     * A list of web portal ARNs that this data protection settings resource is associated with.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The creation date timestamp of the data protection settings.
     *
     * @cloudformationAttribute CreationDate
     */
    readonly attrCreationDate: string;
    /**
     * The ARN of the data protection settings resource.
     *
     * @cloudformationAttribute DataProtectionSettingsArn
     */
    readonly attrDataProtectionSettingsArn: string;
    /**
     * The additional encryption context of the data protection settings.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The customer managed key used to encrypt sensitive information in the data protection settings.
     */
    customerManagedKey?: string;
    /**
     * The description of the data protection settings.
     */
    description?: string;
    /**
     * The display name of the data protection settings.
     */
    displayName?: string;
    /**
     * The inline redaction configuration for the data protection settings.
     */
    inlineRedactionConfiguration?: CfnDataProtectionSettings.InlineRedactionConfigurationProperty | cdk.IResolvable;
    /**
     * The tags of the data protection settings.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnDataProtectionSettingsProps);
    get dataProtectionSettingsRef(): DataProtectionSettingsReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnDataProtectionSettings {
    /**
     * The configuration for in-session inline redaction.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionconfiguration.html
     */
    interface InlineRedactionConfigurationProperty {
        /**
         * The global confidence level for the inline redaction configuration.
         *
         * This indicates the certainty of data type matches in the redaction process. Confidence level 3 means high confidence, and requires a formatted text pattern match in order for content to be redacted. Confidence level 2 means medium confidence, and redaction considers both formatted and unformatted text, and adds keyword associate to the logic. Confidence level 1 means low confidence, and redaction is enforced for both formatted pattern + unformatted pattern without keyword. This is applied to patterns that do not have a pattern-level confidence level. Defaults to confidence level 2.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionconfiguration.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionconfiguration-globalconfidencelevel
         */
        readonly globalConfidenceLevel?: number;
        /**
         * The global enforced URL configuration for the inline redaction configuration.
         *
         * This is applied to patterns that do not have a pattern-level enforced URL list.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionconfiguration.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionconfiguration-globalenforcedurls
         */
        readonly globalEnforcedUrls?: Array<string>;
        /**
         * The global exempt URL configuration for the inline redaction configuration.
         *
         * This is applied to patterns that do not have a pattern-level exempt URL list.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionconfiguration.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionconfiguration-globalexempturls
         */
        readonly globalExemptUrls?: Array<string>;
        /**
         * The inline redaction patterns to be enabled for the inline redaction configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionconfiguration.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionconfiguration-inlineredactionpatterns
         */
        readonly inlineRedactionPatterns: Array<CfnDataProtectionSettings.InlineRedactionPatternProperty | cdk.IResolvable> | cdk.IResolvable;
    }
    /**
     * The set of patterns that determine the data types redacted in session.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html
     */
    interface InlineRedactionPatternProperty {
        /**
         * The built-in pattern from the list of preconfigured patterns.
         *
         * Either a customPattern or builtInPatternId is required. To view the entire list of data types and their corresponding built-in pattern IDs, see [Base inline redaction](https://docs.aws.amazon.com/workspaces-web/latest/adminguide/base-inline-redaction.html) .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-builtinpatternid
         */
        readonly builtInPatternId?: string;
        /**
         * The confidence level for inline redaction pattern.
         *
         * This indicates the certainty of data type matches in the redaction process. Confidence level 3 means high confidence, and requires a formatted text pattern match in order for content to be redacted. Confidence level 2 means medium confidence, and redaction considers both formatted and unformatted text, and adds keyword associate to the logic. Confidence level 1 means low confidence, and redaction is enforced for both formatted pattern + unformatted pattern without keyword. This overrides the global confidence level.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-confidencelevel
         */
        readonly confidenceLevel?: number;
        /**
         * The configuration for a custom pattern.
         *
         * Either a customPattern or builtInPatternId is required.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-custompattern
         */
        readonly customPattern?: CfnDataProtectionSettings.CustomPatternProperty | cdk.IResolvable;
        /**
         * The enforced URL configuration for the inline redaction pattern.
         *
         * This will override the global enforced URL configuration.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-enforcedurls
         */
        readonly enforcedUrls?: Array<string>;
        /**
         * The exempt URL configuration for the inline redaction pattern.
         *
         * This will override the global exempt URL configuration for the inline redaction pattern.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-exempturls
         */
        readonly exemptUrls?: Array<string>;
        /**
         * The redaction placeholder that will replace the redacted text in session for the inline redaction pattern.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-inlineredactionpattern.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionpattern-redactionplaceholder
         */
        readonly redactionPlaceHolder: cdk.IResolvable | CfnDataProtectionSettings.RedactionPlaceHolderProperty;
    }
    /**
     * The pattern configuration for redacting custom data types in session.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-custompattern.html
     */
    interface CustomPatternProperty {
        /**
         * The keyword regex for the customer pattern.
         *
         * After there is a match to the pattern regex, the keyword regex is used to search within the proximity of the match. If there is a keyword match, then the match is confirmed. If no keyword regex is provided, the pattern regex match will automatically be confirmed. The format must follow JavaScript regex format. The pattern must be enclosed between slashes, and can have flags behind the second slash. For example, “/ab+c/gi”
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-custompattern.html#cfn-workspacesweb-dataprotectionsettings-custompattern-keywordregex
         */
        readonly keywordRegex?: string;
        /**
         * The pattern description for the customer pattern.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-custompattern.html#cfn-workspacesweb-dataprotectionsettings-custompattern-patterndescription
         */
        readonly patternDescription?: string;
        /**
         * The pattern name for the custom pattern.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-custompattern.html#cfn-workspacesweb-dataprotectionsettings-custompattern-patternname
         */
        readonly patternName: string;
        /**
         * The pattern regex for the customer pattern.
         *
         * The format must follow JavaScript regex format. The pattern must be enclosed between slashes, and can have flags behind the second slash. For example: “/ab+c/gi”.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-custompattern.html#cfn-workspacesweb-dataprotectionsettings-custompattern-patternregex
         */
        readonly patternRegex: string;
    }
    /**
     * The redaction placeholder that will replace the redacted text in session.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-redactionplaceholder.html
     */
    interface RedactionPlaceHolderProperty {
        /**
         * The redaction placeholder text that will replace the redacted text in session for the custom text redaction placeholder type.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-redactionplaceholder.html#cfn-workspacesweb-dataprotectionsettings-redactionplaceholder-redactionplaceholdertext
         */
        readonly redactionPlaceHolderText?: string;
        /**
         * The redaction placeholder type that will replace the redacted text in session.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-dataprotectionsettings-redactionplaceholder.html#cfn-workspacesweb-dataprotectionsettings-redactionplaceholder-redactionplaceholdertype
         */
        readonly redactionPlaceHolderType: string;
    }
}
/**
 * Properties for defining a `CfnDataProtectionSettings`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html
 */
export interface CfnDataProtectionSettingsProps {
    /**
     * The additional encryption context of the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The customer managed key used to encrypt sensitive information in the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * The description of the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-description
     */
    readonly description?: string;
    /**
     * The display name of the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-displayname
     */
    readonly displayName?: string;
    /**
     * The inline redaction configuration for the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-inlineredactionconfiguration
     */
    readonly inlineRedactionConfiguration?: CfnDataProtectionSettings.InlineRedactionConfigurationProperty | cdk.IResolvable;
    /**
     * The tags of the data protection settings.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-dataprotectionsettings.html#cfn-workspacesweb-dataprotectionsettings-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a DataProtectionSettings resource.
 *
 * @struct
 * @stability external
 */
export interface DataProtectionSettingsReference {
    /**
     * The DataProtectionSettingsArn of the DataProtectionSettings resource.
     */
    readonly dataProtectionSettingsArn: string;
}
/**
 * Indicates that this resource can be referenced as a SessionLogger.
 *
 * @stability experimental
 */
export interface ISessionLoggerRef extends constructs.IConstruct {
    /**
     * A reference to a SessionLogger resource.
     */
    readonly sessionLoggerRef: SessionLoggerReference;
}
/**
 * The session logger resource.
 *
 * @cloudformationResource AWS::WorkSpacesWeb::SessionLogger
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html
 */
export declare class CfnSessionLogger extends cdk.CfnResource implements cdk.IInspectable, ISessionLoggerRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnSessionLogger from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnSessionLogger;
    /**
     * The associated portal ARN.
     *
     * @cloudformationAttribute AssociatedPortalArns
     */
    readonly attrAssociatedPortalArns: Array<string>;
    /**
     * The date the session logger resource was created.
     *
     * @cloudformationAttribute CreationDate
     */
    readonly attrCreationDate: string;
    /**
     * The ARN of the session logger resource.
     *
     * @cloudformationAttribute SessionLoggerArn
     */
    readonly attrSessionLoggerArn: string;
    /**
     * The additional encryption context of the session logger.
     */
    additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The custom managed key of the session logger.
     */
    customerManagedKey?: string;
    /**
     * The human-readable display name.
     */
    displayName?: string;
    /**
     * The filter that specifies which events to monitor.
     */
    eventFilter: CfnSessionLogger.EventFilterProperty | cdk.IResolvable;
    /**
     * The configuration that specifies where logs are fowarded.
     */
    logConfiguration: cdk.IResolvable | CfnSessionLogger.LogConfigurationProperty;
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnSessionLoggerProps);
    get sessionLoggerRef(): SessionLoggerReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnSessionLogger {
    /**
     * The filter that specifies the events to monitor.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-eventfilter.html
     */
    interface EventFilterProperty {
        /**
         * The filter that monitors all of the available events, including any new events emitted in the future.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-eventfilter.html#cfn-workspacesweb-sessionlogger-eventfilter-all
         */
        readonly all?: any | cdk.IResolvable;
        /**
         * The filter that monitors only the listed set of events.
         *
         * New events are not auto-monitored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-eventfilter.html#cfn-workspacesweb-sessionlogger-eventfilter-include
         */
        readonly include?: Array<string>;
    }
    /**
     * The configuration of the log.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-logconfiguration.html
     */
    interface LogConfigurationProperty {
        /**
         * The configuration for delivering the logs to S3.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-logconfiguration.html#cfn-workspacesweb-sessionlogger-logconfiguration-s3
         */
        readonly s3?: cdk.IResolvable | CfnSessionLogger.S3LogConfigurationProperty;
    }
    /**
     * The S3 log configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html
     */
    interface S3LogConfigurationProperty {
        /**
         * The S3 bucket name where logs are delivered.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html#cfn-workspacesweb-sessionlogger-s3logconfiguration-bucket
         */
        readonly bucket: string;
        /**
         * The expected bucket owner of the target S3 bucket.
         *
         * The caller must have permissions to write to the target bucket.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html#cfn-workspacesweb-sessionlogger-s3logconfiguration-bucketowner
         */
        readonly bucketOwner?: string;
        /**
         * The folder structure that defines the organizational structure for log files in S3.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html#cfn-workspacesweb-sessionlogger-s3logconfiguration-folderstructure
         */
        readonly folderStructure: string;
        /**
         * The S3 path prefix that determines where log files are stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html#cfn-workspacesweb-sessionlogger-s3logconfiguration-keyprefix
         */
        readonly keyPrefix?: string;
        /**
         * The format of the LogFile that is written to S3.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-workspacesweb-sessionlogger-s3logconfiguration.html#cfn-workspacesweb-sessionlogger-s3logconfiguration-logfileformat
         */
        readonly logFileFormat: string;
    }
}
/**
 * Properties for defining a `CfnSessionLogger`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html
 */
export interface CfnSessionLoggerProps {
    /**
     * The additional encryption context of the session logger.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-additionalencryptioncontext
     */
    readonly additionalEncryptionContext?: cdk.IResolvable | Record<string, string>;
    /**
     * The custom managed key of the session logger.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-customermanagedkey
     */
    readonly customerManagedKey?: string;
    /**
     * The human-readable display name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-displayname
     */
    readonly displayName?: string;
    /**
     * The filter that specifies which events to monitor.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-eventfilter
     */
    readonly eventFilter: CfnSessionLogger.EventFilterProperty | cdk.IResolvable;
    /**
     * The configuration that specifies where logs are fowarded.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-logconfiguration
     */
    readonly logConfiguration: cdk.IResolvable | CfnSessionLogger.LogConfigurationProperty;
    /**
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-workspacesweb-sessionlogger.html#cfn-workspacesweb-sessionlogger-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a SessionLogger resource.
 *
 * @struct
 * @stability external
 */
export interface SessionLoggerReference {
    /**
     * The SessionLoggerArn of the SessionLogger resource.
     */
    readonly sessionLoggerArn: string;
}
