import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a BillingView.
 *
 * @stability experimental
 */
export interface IBillingViewRef extends constructs.IConstruct {
    /**
     * A reference to a BillingView resource.
     */
    readonly billingViewRef: BillingViewReference;
}
/**
 * Creates a billing view with the specified billing view attributes.
 *
 * @cloudformationResource AWS::Billing::BillingView
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html
 */
export declare class CfnBillingView extends cdk.CfnResource implements cdk.IInspectable, IBillingViewRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnBillingView from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnBillingView;
    /**
     * The Amazon Resource Name (ARN) that can be used to uniquely identify the billing view.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The type of billing group.
     *
     * @cloudformationAttribute BillingViewType
     */
    readonly attrBillingViewType: string;
    /**
     * The time when the billing view was created.
     *
     * @cloudformationAttribute CreatedAt
     */
    readonly attrCreatedAt: cdk.IResolvable;
    /**
     * The account owner of the billing view.
     *
     * @cloudformationAttribute OwnerAccountId
     */
    readonly attrOwnerAccountId: string;
    /**
     * The time when the billing view was last updated.
     *
     * @cloudformationAttribute UpdatedAt
     */
    readonly attrUpdatedAt: cdk.IResolvable;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * See [Expression](https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_billing_Expression.html) . Billing view only supports `LINKED_ACCOUNT` and `Tags` .
     */
    dataFilterExpression?: CfnBillingView.DataFilterExpressionProperty | cdk.IResolvable;
    /**
     * The description of the billing view.
     */
    description?: string;
    /**
     * The name of the billing view.
     */
    name: string;
    /**
     * A list of billing views used as the data source for the custom billing view.
     */
    sourceViews: Array<string>;
    /**
     * A list of key value map specifying tags associated to the billing view being created.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnBillingViewProps);
    get billingViewRef(): BillingViewReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnBillingView {
    /**
     * See [Expression](https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_billing_Expression.html) . Billing view only supports `LINKED_ACCOUNT` and `Tags` .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-datafilterexpression.html
     */
    interface DataFilterExpressionProperty {
        /**
         * The specific `Dimension` to use for `Expression` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-datafilterexpression.html#cfn-billing-billingview-datafilterexpression-dimensions
         */
        readonly dimensions?: CfnBillingView.DimensionsProperty | cdk.IResolvable;
        /**
         * The specific `Tag` to use for `Expression` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-datafilterexpression.html#cfn-billing-billingview-datafilterexpression-tags
         */
        readonly tags?: CfnBillingView.TagsProperty;
    }
    /**
     * The specific `Dimension` to use for `Expression` .
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-dimensions.html
     */
    interface DimensionsProperty {
        /**
         * The key that's associated with the tag.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-dimensions.html#cfn-billing-billingview-dimensions-key
         */
        readonly key?: string;
        /**
         * The metadata that you can use to filter and group your results.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-dimensions.html#cfn-billing-billingview-dimensions-values
         */
        readonly values?: Array<string>;
    }
    /**
     * Tags associated with the billing view resource.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-tags.html
     */
    interface TagsProperty {
        /**
         * A list of tag key value pairs that are associated with the resource.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-tags.html#cfn-billing-billingview-tags-key
         */
        readonly key?: string;
        /**
         * The metadata values that you can use to filter and group your results.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-billing-billingview-tags.html#cfn-billing-billingview-tags-values
         */
        readonly values?: Array<string>;
    }
}
/**
 * Properties for defining a `CfnBillingView`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html
 */
export interface CfnBillingViewProps {
    /**
     * See [Expression](https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_billing_Expression.html) . Billing view only supports `LINKED_ACCOUNT` and `Tags` .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html#cfn-billing-billingview-datafilterexpression
     */
    readonly dataFilterExpression?: CfnBillingView.DataFilterExpressionProperty | cdk.IResolvable;
    /**
     * The description of the billing view.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html#cfn-billing-billingview-description
     */
    readonly description?: string;
    /**
     * The name of the billing view.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html#cfn-billing-billingview-name
     */
    readonly name: string;
    /**
     * A list of billing views used as the data source for the custom billing view.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html#cfn-billing-billingview-sourceviews
     */
    readonly sourceViews: Array<string>;
    /**
     * A list of key value map specifying tags associated to the billing view being created.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-billing-billingview.html#cfn-billing-billingview-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a BillingView resource.
 *
 * @struct
 * @stability external
 */
export interface BillingViewReference {
    /**
     * The Arn of the BillingView resource.
     */
    readonly billingViewArn: string;
}
