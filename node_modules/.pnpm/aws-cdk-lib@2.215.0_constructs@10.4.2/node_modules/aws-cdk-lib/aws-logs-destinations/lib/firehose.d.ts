import { Construct } from 'constructs';
import * as iam from '../../aws-iam';
import * as firehose from '../../aws-kinesisfirehose';
import * as logs from '../../aws-logs';
/**
 * Customize the Amazon Data Firehose Logs Destination
 */
export interface FirehoseDestinationProps {
    /**
     * The role to assume to write log events to the destination
     *
     * @default - A new Role is created
     */
    readonly role?: iam.IRole;
}
/**
 * Use a Data Firehose delivery stream as the destination for a log subscription
 */
export declare class FirehoseDestination implements logs.ILogSubscriptionDestination {
    private readonly stream;
    private readonly props;
    /**
     * @param stream The Data Firehose delivery stream to use as destination
     * @param props The Data Firehose Destination properties
     *
     */
    constructor(stream: firehose.IDeliveryStream, props?: FirehoseDestinationProps);
    bind(scope: Construct, _sourceLogGroup: logs.ILogGroup): logs.LogSubscriptionDestinationConfig;
}
