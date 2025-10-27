import { HttpOrigin, HttpOriginProps } from './http-origin';
import { IBucket } from '../../aws-s3';
/**
 * Properties for configuring a origin using a S3 bucket configured as a website endpoint
 */
export interface S3StaticWebsiteOriginProps extends HttpOriginProps {
}
/**
 * An Origin for a S3 bucket configured as a website endpoint
 */
export declare class S3StaticWebsiteOrigin extends HttpOrigin {
    constructor(bucket: IBucket, props?: S3StaticWebsiteOriginProps);
}
