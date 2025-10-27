import { IConstruct, IDependable } from 'constructs';
import { PolicyStatement } from './policy-statement';
import { IGrantable, IPrincipal } from './principals';
import * as cdk from '../../core';
/**
 * Basic options for a grant operation
 *
 */
export interface CommonGrantOptions {
    /**
     * The principal to grant to
     *
     * @default if principal is undefined, no work is done.
     */
    readonly grantee: IGrantable;
    /**
     * The actions to grant
     */
    readonly actions: string[];
    /**
     * The resource ARNs to grant to
     */
    readonly resourceArns: string[];
    /**
     * Any conditions to attach to the grant
     *
     * @default - No conditions
     */
    readonly conditions?: Record<string, Record<string, unknown>>;
}
/**
 * Options for a grant operation
 *
 */
export interface GrantWithResourceOptions extends CommonGrantOptions {
    /**
     * The resource with a resource policy
     *
     * The statement will be added to the resource policy if it couldn't be
     * added to the principal policy.
     */
    readonly resource: IResourceWithPolicy;
    /**
     * When referring to the resource in a resource policy, use this as ARN.
     *
     * (Depending on the resource type, this needs to be '*' in a resource policy).
     *
     * @default Same as regular resource ARNs
     */
    readonly resourceSelfArns?: string[];
}
/**
 * Options for a grant operation that directly adds a policy statement to a resource
 *
 * This differs from GrantWithResourceOptions in that it requires a pre-constructed
 * PolicyStatement rather than constructing one from individual permissions.
 * Use this when you need fine-grained control over the initial policy statement's contents.
 */
export interface GrantPolicyWithResourceOptions extends GrantWithResourceOptions {
    /**
     * The policy statement to add to the resource's policy
     *
     * This statement will be passed to the resource's addToResourcePolicy method.
     * The actual handling of the statement depends on the specific IResourceWithPolicy
     * implementation.
     */
    readonly statement: PolicyStatement;
}
/**
 * Options for a grant operation that only applies to principals
 *
 */
export interface GrantOnPrincipalOptions extends CommonGrantOptions {
    /**
     * Construct to report warnings on in case grant could not be registered
     *
     * @default - the construct in which this construct is defined
     */
    readonly scope?: IConstruct;
}
/**
 * Options for a grant operation to both identity and resource
 *
 */
export interface GrantOnPrincipalAndResourceOptions extends CommonGrantOptions {
    /**
     * The resource with a resource policy
     *
     * The statement will always be added to the resource policy.
     */
    readonly resource: IResourceWithPolicy;
    /**
     * When referring to the resource in a resource policy, use this as ARN.
     *
     * (Depending on the resource type, this needs to be '*' in a resource policy).
     *
     * @default Same as regular resource ARNs
     */
    readonly resourceSelfArns?: string[];
    /**
     * The principal to use in the statement for the resource policy.
     *
     * @default - the principal of the grantee will be used
     */
    readonly resourcePolicyPrincipal?: IPrincipal;
}
/**
 * Result of a grant() operation
 *
 * This class is not instantiable by consumers on purpose, so that they will be
 * required to call the Grant factory functions.
 */
export declare class Grant implements IDependable {
    /**
     * Grant the given permissions to the principal
     *
     * The permissions will be added to the principal policy primarily, falling
     * back to the resource policy if necessary. The permissions must be granted
     * somewhere.
     *
     * - Trying to grant permissions to a principal that does not admit adding to
     *   the principal policy while not providing a resource with a resource policy
     *   is an error.
     * - Trying to grant permissions to an absent principal (possible in the
     *   case of imported resources) leads to a warning being added to the
     *   resource construct.
     */
    static addToPrincipalOrResource(options: GrantWithResourceOptions): Grant;
    /**
     * Add a pre-constructed policy statement to the resource's policy
     *
     * This method provides direct, low-level control over the initial policy statement being added.
     * It is useful when you need to:
     * - Add complex policy statements that can't be expressed through other grant methods
     * - Specify the initial structure of the policy statement
     * - Add statements with custom conditions or other advanced IAM features
     *
     * Important differences from other grant methods:
     * - Only modifies the resource policy, never modifies any principal's policy
     * - Takes a complete PolicyStatement rather than constructing one from parameters
     * - Always attempts to add the statement, regardless of principal type or account
     * - Does not attempt any automatic principal/resource policy selection logic
     *
     * Note: The final form of the policy statement in the resource's policy may differ
     * from the provided statement, depending on the resource's implementation of
     * addToResourcePolicy.
     *
     * @param options Contains both the target resource and the policy statement to add
     * @returns A Grant object representing the result of the operation
     *
     * @example
     *
     * declare const grantee: iam.IGrantable;
     * declare const actions: string[];
     * declare const resourceArns: string[];
     * declare const bucket: s3.Bucket;
     *
     * const statement = new iam.PolicyStatement({
     *   effect: iam.Effect.ALLOW,
     *   actions: actions,
     *   principals: [new iam.ServicePrincipal('lambda.amazonaws.com')],
     *   conditions: {
     *     StringEquals: {
     *       'aws:SourceAccount': Stack.of(this).account,
     *     },
     *   },
     * });
     * iam.Grant.addStatementToResourcePolicy({
     *     grantee: grantee,
     *     actions: actions,
     *     resourceArns: resourceArns,
     *     resource: bucket,
     *     statement: statement,
     *  });
     *
     */
    static addStatementToResourcePolicy(options: GrantPolicyWithResourceOptions): Grant;
    /**
     * Try to grant the given permissions to the given principal
     *
     * Absence of a principal leads to a warning, but failing to add
     * the permissions to a present principal is not an error.
     */
    static addToPrincipal(options: GrantOnPrincipalOptions): Grant;
    /**
     * Add a grant both on the principal and on the resource
     *
     * As long as any principal is given, granting on the principal may fail (in
     * case of a non-identity principal), but granting on the resource will
     * never fail.
     *
     * Statement will be the resource statement.
     */
    static addToPrincipalAndResource(options: GrantOnPrincipalAndResourceOptions): Grant;
    /**
     * Returns a "no-op" `Grant` object which represents a "dropped grant".
     *
     * This can be used for e.g. imported resources where you may not be able to modify
     * the resource's policy or some underlying policy which you don't know about.
     *
     * @param grantee The intended grantee
     * @param _intent The user's intent (will be ignored at the moment)
     */
    static drop(grantee: IGrantable, _intent: string): Grant;
    /**
     * The statement that was added to the principal's policy
     *
     * @deprecated Use `principalStatements` instead
     */
    readonly principalStatement?: PolicyStatement;
    /**
     * The statements that were added to the principal's policy
     */
    readonly principalStatements: PolicyStatement[];
    /**
     * The statement that was added to the resource policy
     *
     * @deprecated Use `resourceStatements` instead
     */
    readonly resourceStatement?: PolicyStatement;
    /**
     * The statements that were added to the resource policy
     */
    readonly resourceStatements: PolicyStatement[];
    /**
     * The options originally used to set this result
     *
     * Private member doubles as a way to make it impossible for an object literal to
     * be structurally the same as this class.
     */
    private readonly options;
    private readonly dependables;
    private constructor();
    /**
     * Whether the grant operation was successful
     */
    get success(): boolean;
    /**
     * Throw an error if this grant wasn't successful
     */
    assertSuccess(): void;
    /**
     * Make sure this grant is applied before the given constructs are deployed
     *
     * The same as construct.node.addDependency(grant), but slightly nicer to read.
     */
    applyBefore(...constructs: IConstruct[]): void;
    /**
     * Combine two grants into a new one
     */
    combine(rhs: Grant): Grant;
}
/**
 * A resource with a resource policy that can be added to
 */
export interface IResourceWithPolicy extends cdk.IResource {
    /**
     * Add a statement to the resource's resource policy
     */
    addToResourcePolicy(statement: PolicyStatement): AddToResourcePolicyResult;
}
/**
 * Result of calling addToResourcePolicy
 */
export interface AddToResourcePolicyResult {
    /**
     * Whether the statement was added
     */
    readonly statementAdded: boolean;
    /**
     * Dependable which allows depending on the policy change being applied
     *
     * @default - If `statementAdded` is true, the resource object itself.
     * Otherwise, no dependable.
     */
    readonly policyDependable?: IDependable;
}
/**
 * Composite dependable
 *
 * Not as simple as eagerly getting the dependency roots from the
 * inner dependables, as they may be mutable so we need to defer
 * the query.
 */
export declare class CompositeDependable implements IDependable {
    constructor(...dependables: IDependable[]);
}
