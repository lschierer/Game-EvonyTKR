import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a InvoiceUnit.
 *
 * @stability experimental
 */
export interface IInvoiceUnitRef extends constructs.IConstruct {
    /**
     * A reference to a InvoiceUnit resource.
     */
    readonly invoiceUnitRef: InvoiceUnitReference;
}
/**
 * An invoice unit is a set of mutually exclusive account that correspond to your business entity.
 *
 * Invoice units allow you separate AWS account costs and configures your invoice for each business entity going forward.
 *
 * @cloudformationResource AWS::Invoicing::InvoiceUnit
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html
 */
export declare class CfnInvoiceUnit extends cdk.CfnResource implements cdk.IInspectable, IInvoiceUnitRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnInvoiceUnit from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnInvoiceUnit;
    /**
     * The ARN to identify an invoice unit. This information can't be modified or deleted.
     *
     * @cloudformationAttribute InvoiceUnitArn
     */
    readonly attrInvoiceUnitArn: string;
    /**
     * The last time the invoice unit was updated. This is important to determine the version of invoice unit configuration used to create the invoices. Any invoice created after this modified time will use this invoice unit configuration.
     *
     * @cloudformationAttribute LastModified
     */
    readonly attrLastModified: cdk.IResolvable;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * The assigned description for an invoice unit.
     */
    description?: string;
    /**
     * The account that receives invoices related to the invoice unit.
     */
    invoiceReceiver: string;
    /**
     * A unique name that is distinctive within your AWS .
     */
    name: string;
    /**
     * The tag structure that contains a tag key and value.
     */
    resourceTags?: Array<CfnInvoiceUnit.ResourceTagProperty>;
    /**
     * An `InvoiceUnitRule` object used the categorize invoice units.
     */
    rule: cdk.IResolvable | CfnInvoiceUnit.RuleProperty;
    /**
     * Whether the invoice unit based tax inheritance is/ should be enabled or disabled.
     */
    taxInheritanceDisabled?: boolean | cdk.IResolvable;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnInvoiceUnitProps);
    get invoiceUnitRef(): InvoiceUnitReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnInvoiceUnit {
    /**
     * The `InvoiceUnitRule` object used to update invoice units.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-invoicing-invoiceunit-rule.html
     */
    interface RuleProperty {
        /**
         * The list of `LINKED_ACCOUNT` IDs where charges are included within the invoice unit.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-invoicing-invoiceunit-rule.html#cfn-invoicing-invoiceunit-rule-linkedaccounts
         */
        readonly linkedAccounts: Array<string>;
    }
    /**
     * The tag structure that contains a tag key and value.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-invoicing-invoiceunit-resourcetag.html
     */
    interface ResourceTagProperty {
        /**
         * The object key of your of your resource tag.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-invoicing-invoiceunit-resourcetag.html#cfn-invoicing-invoiceunit-resourcetag-key
         */
        readonly key: string;
        /**
         * The specific value of the resource tag.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-invoicing-invoiceunit-resourcetag.html#cfn-invoicing-invoiceunit-resourcetag-value
         */
        readonly value: string;
    }
}
/**
 * Properties for defining a `CfnInvoiceUnit`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html
 */
export interface CfnInvoiceUnitProps {
    /**
     * The assigned description for an invoice unit.
     *
     * This information can't be modified or deleted.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-description
     */
    readonly description?: string;
    /**
     * The account that receives invoices related to the invoice unit.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-invoicereceiver
     */
    readonly invoiceReceiver: string;
    /**
     * A unique name that is distinctive within your AWS .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-name
     */
    readonly name: string;
    /**
     * The tag structure that contains a tag key and value.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-resourcetags
     */
    readonly resourceTags?: Array<CfnInvoiceUnit.ResourceTagProperty>;
    /**
     * An `InvoiceUnitRule` object used the categorize invoice units.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-rule
     */
    readonly rule: cdk.IResolvable | CfnInvoiceUnit.RuleProperty;
    /**
     * Whether the invoice unit based tax inheritance is/ should be enabled or disabled.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-invoicing-invoiceunit.html#cfn-invoicing-invoiceunit-taxinheritancedisabled
     */
    readonly taxInheritanceDisabled?: boolean | cdk.IResolvable;
}
/**
 * A reference to a InvoiceUnit resource.
 *
 * @struct
 * @stability external
 */
export interface InvoiceUnitReference {
    /**
     * The InvoiceUnitArn of the InvoiceUnit resource.
     */
    readonly invoiceUnitArn: string;
}
