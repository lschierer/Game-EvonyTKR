import { Construct } from 'constructs';
import { CfnAssessmentTemplate } from './inspector.generated';
import { IResource, Resource } from '../../core';
/**
 * Interface for an Inspector Assessment Template
 */
export interface IAssessmentTemplate extends IResource {
    /**
     * The Amazon Resource Name (ARN) of the assessment template.
     * @attribute
     */
    readonly assessmentTemplateArn: string;
}
/**
 * Properties for creating an Inspector Assessment Template
 * TODO: Add properties and remove "props-physical-name:aws-cdk-lib.aws_inspector.AssessmentTemplateProps" from `awslint.json`
 * when implementing the L2 construct
 */
export interface AssessmentTemplateProps {
}
/**
 * An Amazon Inspector assessment template.
 * TODO: This class should implement IAssessmentTemplate and "construct-ctor-props-type:aws-cdk-lib.aws_inspector.AssessmentTemplate" should be
 * removed from `awslint.json` when implementing the L2 construct
 */
export declare class AssessmentTemplate extends Resource {
    /** Uniquely identifies this class. */
    static readonly PROPERTY_INJECTION_ID: string;
    /**
     * Creates an AssessmentTemplate from an existing CfnAssessmentTemplate.
     *
     * This method is provided to bridge the gap with L2 constructs since no L2 constructs
     * exist for Inspector resources yet. It allows working with CfnAssessmentTemplate (L1)
     * resources through the IAssessmentTemplate interface.
     */
    static fromCfnAssessmentTemplate(scope: Construct, id: string, template: CfnAssessmentTemplate): IAssessmentTemplate;
}
