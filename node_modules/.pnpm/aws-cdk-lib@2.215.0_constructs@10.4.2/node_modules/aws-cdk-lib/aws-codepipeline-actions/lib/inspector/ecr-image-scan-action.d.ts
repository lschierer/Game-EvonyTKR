import { Construct } from 'constructs';
import { InspectorScanActionBase, InspectorScanActionBaseProps } from './scan-action-base';
import * as codepipeline from '../../../aws-codepipeline';
import * as ecr from '../../../aws-ecr';
/**
 * Construction properties of the `InspectorEcrImageScanAction`.
 */
export interface InspectorEcrImageScanActionProps extends InspectorScanActionBaseProps {
    /**
     * The Amazon ECR repository where the image is pushed.
     */
    readonly repository: ecr.IRepository;
    /**
     * The tag used for the image.
     *
     * @default 'latest'
     */
    readonly imageTag?: string;
}
/**
 * CodePipeline invoke action that uses AWS InspectorScan for ECR images.
 */
export declare class InspectorEcrImageScanAction extends InspectorScanActionBase {
    private readonly ecrProps;
    constructor(props: InspectorEcrImageScanActionProps);
    protected renderActionConfiguration(): Record<string, any>;
    protected bound(scope: Construct, stage: codepipeline.IStage, options: codepipeline.ActionBindOptions): codepipeline.ActionConfig;
}
