import { Construct } from 'constructs';
import { DataProcessorBindOptions, DataProcessorConfig, DataProcessorProps, IDataProcessor } from './processor';
import * as lambda from '../../aws-lambda';
/**
 * Use an AWS Lambda function to transform records.
 */
export declare class LambdaFunctionProcessor implements IDataProcessor {
    private readonly lambdaFunction;
    /**
     * The constructor props of the LambdaFunctionProcessor.
     */
    readonly props: DataProcessorProps;
    constructor(lambdaFunction: lambda.IFunction, props?: DataProcessorProps);
    bind(_scope: Construct, options: DataProcessorBindOptions): DataProcessorConfig;
}
