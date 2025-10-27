import { Construct } from 'constructs';
import { IStream } from './stream';
import * as iam from '../../aws-iam';
import { IResource, Resource } from '../../core';
/**
 * A Kinesis Stream Consumer
 */
export interface IStreamConsumer extends IResource {
    /**
     * The ARN of the stream consumer.
     *
     * @attribute
     */
    readonly streamConsumerArn: string;
    /**
     * The name of the stream consumer.
     *
     * @attribute
     */
    readonly streamConsumerName: string;
    /**
     * The stream associated with this consumer.
     *
     * @attribute
     */
    readonly stream: IStream;
    /**
     * Adds a statement to the IAM resource policy associated with this stream consumer.
     *
     * If this stream consumer was created in this stack (`new StreamConsumer`), a resource policy
     * will be automatically created upon the first call to `addToResourcePolicy`. If
     * the stream consumer is imported (`StreamConsumer.from`), then this is a no-op.
     */
    addToResourcePolicy(statement: iam.PolicyStatement): iam.AddToResourcePolicyResult;
    /**
     * Grant read permissions for this stream consumer and its associated stream to an IAM
     * principal (Role/Group/User).
     */
    grantRead(grantee: iam.IGrantable): iam.Grant;
    /**
     * Grant the indicated permissions on this stream consumer to the provided IAM principal.
     */
    grant(grantee: iam.IGrantable, ...actions: string[]): iam.Grant;
}
declare abstract class StreamConsumerBase extends Resource implements IStreamConsumer {
    /**
     * The ARN of the stream consumer.
     */
    abstract readonly streamConsumerArn: string;
    /**
     * The name of the stream consumer.
     */
    abstract readonly streamConsumerName: string;
    /**
     * The Kinesis data stream this consumer is associated with.
     */
    abstract readonly stream: IStream;
    /**
     * Indicates if a resource policy should automatically be created upon
     * the first call to `addToResourcePolicy`.
     *
     * Set by subclasses.
     */
    protected abstract readonly autoCreatePolicy: boolean;
    private resourcePolicy?;
    /**
     * Adds a statement to the IAM resource policy associated with this stream consumer.
     *
     * If this stream consumer was created in this stack (`new StreamConsumer`), a resource policy
     * will be automatically created upon the first call to `addToResourcePolicy`. If
     * the stream is imported (`StreamConsumer.from`), then this is a no-op.
     */
    addToResourcePolicy(statement: iam.PolicyStatement): iam.AddToResourcePolicyResult;
    /**
     * Grant read permissions for this stream consumer and its associated stream to an IAM
     * principal (Role/Group/User).
     */
    grantRead(grantee: iam.IGrantable): iam.Grant;
    /**
     * Grant the indicated permissions on this stream consumer to the given IAM principal (Role/Group/User).
     */
    grant(grantee: iam.IGrantable, ...actions: string[]): iam.Grant;
}
/**
 * A reference to a StreamConsumer, which can be imported using `StreamConsumer.fromStreamConsumerAttributes`.
 */
export interface StreamConsumerAttributes {
    /**
     * The Amazon Resource Name (ARN) of the stream consumer.
     */
    readonly streamConsumerArn: string;
}
/**
 * Properties for a Kinesis Stream Consumer.
 */
export interface StreamConsumerProps {
    /**
     * The name of the stream consumer.
     */
    readonly streamConsumerName: string;
    /**
     * The Kinesis data stream to associate this consumer with.
     */
    readonly stream: IStream;
}
/**
 * A Kinesis Stream Consumer
 */
export declare class StreamConsumer extends StreamConsumerBase {
    /** Uniquely identifies this class. */
    static readonly PROPERTY_INJECTION_ID: string;
    /**
     * Imports an existing Kinesis Stream Consumer by its arn.
     *
     * @param scope the Construct scope.
     * @param id the ID of the construct.
     * @param streamConsumerArn the arn of the existing stream consumer.
     */
    static fromStreamConsumerArn(scope: Construct, id: string, streamConsumerArn: string): IStreamConsumer;
    /**
     * Imports an existing Kinesis Stream Consumer by its attributes.
     *
     * @param scope the Construct scope.
     * @param id the ID of the construct.
     * @param attrs the attributes of the existing stream consumer.
     */
    static fromStreamConsumerAttributes(scope: Construct, id: string, attrs: StreamConsumerAttributes): IStreamConsumer;
    /**
     * The Amazon Resource Name (ARN) of the stream consumer.
     */
    readonly streamConsumerArn: string;
    /**
     * The name of the stream consumer.
     */
    readonly streamConsumerName: string;
    /**
     * The Kinesis data stream this consumer is associated with.
     */
    readonly stream: IStream;
    protected readonly autoCreatePolicy = true;
    constructor(scope: Construct, id: string, props: StreamConsumerProps);
}
export {};
