import { Construct, IConstruct } from 'constructs';
import { Resource } from './resource';
/**
 * Enumeration of metadata types used for tracking analytics in AWS CDK.
 */
export declare enum MetadataType {
    /**
     * Metadata type for construct properties.
     * This is used to represent properties of CDK constructs.
     */
    CONSTRUCT = "aws:cdk:analytics:construct",
    /**
     * Metadata type for method properties.
     * This is used to track parameters and details of CDK method calls.
     */
    METHOD = "aws:cdk:analytics:method",
    /**
     * Metadata type for feature flags.
     * This is used to track analytics related to feature flags in the CDK.
     */
    FEATURE_FLAG = "aws:cdk:analytics:featureflag"
}
export declare function addConstructMetadata(scope: Construct, props: any): void;
export declare function addMethodMetadata(scope: Construct, methodName: string, props: any): void;
/**
 * Method decorator for tracking analytics metadata.
 * This decorator is used to track method calls in the CDK.
 */
export declare function MethodMetadata(): MethodDecorator;
export declare function addMetadata(scope: Construct, type: MetadataType, props: any): void;
/**
 * Check whether the given construct is a Resource. Note that this is
 * duplicated function from 'core/lib/resource.ts' to avoid circular
 * dependencies in imports.
 */
export declare function isResource(construct: IConstruct): construct is Resource;
/**
 * Redact values from dictionary values other than Boolean and ENUM-type values.
 * @TODO we will build a JSON blueprint of ENUM-type values in the codebase and
 * do not redact the ENUM-type values if it match any key in the blueprint.
 */
export declare function redactMetadata(fqn: string, data: any): any;
export declare function redactTelemetryDataHelper(allowedKeys: any, data: any): any;
/**
 * Check if a value is an ENUM and matches the ENUM blueprint.
 */
export declare function isEnumValue(allowedKeys: any, value: any): boolean;
