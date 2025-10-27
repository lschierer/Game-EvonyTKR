import { Construct } from 'constructs';
import { CommonDestinationProps, CommonDestinationS3Props } from './common';
import { DestinationBindOptions, DestinationConfig, IDestination } from './destination';
import * as s3 from '../../aws-s3';
import { TimeZone } from '../../core';
/**
 * Props for defining an S3 destination of an Amazon Data Firehose delivery stream.
 */
export interface S3BucketProps extends CommonDestinationS3Props, CommonDestinationProps {
    /**
     * Specify a file extension.
     * It will override the default file extension appended by Data Format Conversion or S3 compression features such as `.parquet` or `.gz`.
     *
     * File extension must start with a period (`.`) and can contain allowed characters: `0-9a-z!-_.*'()`.
     *
     * @see https://docs.aws.amazon.com/firehose/latest/dev/create-destination.html#create-destination-s3
     * @default - The default file extension appended by Data Format Conversion or S3 compression features
     */
    readonly fileExtension?: string;
    /**
     * The time zone you prefer.
     *
     * @see https://docs.aws.amazon.com/firehose/latest/dev/s3-prefixes.html#timestamp-namespace
     *
     * @default - UTC
     */
    readonly timeZone?: TimeZone;
}
/**
 * An S3 bucket destination for data from an Amazon Data Firehose delivery stream.
 */
export declare class S3Bucket implements IDestination {
    private readonly bucket;
    private readonly props;
    constructor(bucket: s3.IBucket, props?: S3BucketProps);
    bind(scope: Construct, _options: DestinationBindOptions): DestinationConfig;
    private getS3BackupMode;
}
