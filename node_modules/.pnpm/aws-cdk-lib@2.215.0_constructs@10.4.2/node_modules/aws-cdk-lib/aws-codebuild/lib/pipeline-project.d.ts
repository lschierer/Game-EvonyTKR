import { Construct } from 'constructs';
import { CommonProjectProps, Project } from './project';
export interface PipelineProjectProps extends CommonProjectProps {
}
/**
 * A convenience class for CodeBuild Projects that are used in CodePipeline.
 */
export declare class PipelineProject extends Project {
    /** Uniquely identifies this class. */
    static readonly PROPERTY_INJECTION_ID: string;
    constructor(scope: Construct, id: string, props?: PipelineProjectProps);
}
