export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class IoTAnalyticsMetrics {
    static incomingMessagesSum(this: void, dimensions: {
        ChannelName: string;
    }): MetricWithDims<{
        ChannelName: string;
    }>;
    static incomingMessagesSum(this: void, dimensions: {
        DatasetName: string;
    }): MetricWithDims<{
        DatasetName: string;
    }>;
    static incomingMessagesSum(this: void, dimensions: {
        DatastoreName: string;
    }): MetricWithDims<{
        DatastoreName: string;
    }>;
    static incomingMessagesSum(this: void, dimensions: {
        PipelineActivityName: string;
    }): MetricWithDims<{
        PipelineActivityName: string;
    }>;
    static actionExecutionSum(this: void, dimensions: {
        DatasetName: string;
    }): MetricWithDims<{
        DatasetName: string;
    }>;
    static actionExecutionSum(this: void, dimensions: {
        DatastoreName: string;
    }): MetricWithDims<{
        DatastoreName: string;
    }>;
    static actionExecutionSum(this: void, dimensions: {
        PipelineActivityName: string;
    }): MetricWithDims<{
        PipelineActivityName: string;
    }>;
    static actionExecutionThrottledSum(this: void, dimensions: {
        DatasetName: string;
    }): MetricWithDims<{
        DatasetName: string;
    }>;
    static actionExecutionThrottledSum(this: void, dimensions: {
        DatastoreName: string;
    }): MetricWithDims<{
        DatastoreName: string;
    }>;
    static actionExecutionThrottledSum(this: void, dimensions: {
        PipelineActivityName: string;
    }): MetricWithDims<{
        PipelineActivityName: string;
    }>;
    static activityExecutionErrorSum(this: void, dimensions: {
        DatasetName: string;
    }): MetricWithDims<{
        DatasetName: string;
    }>;
    static activityExecutionErrorSum(this: void, dimensions: {
        DatastoreName: string;
    }): MetricWithDims<{
        DatastoreName: string;
    }>;
    static activityExecutionErrorSum(this: void, dimensions: {
        PipelineActivityName: string;
    }): MetricWithDims<{
        PipelineActivityName: string;
    }>;
    static pipelineConcurrentExecutionCountSum(this: void, dimensions: {
        DatasetName: string;
    }): MetricWithDims<{
        DatasetName: string;
    }>;
    static pipelineConcurrentExecutionCountSum(this: void, dimensions: {
        DatastoreName: string;
    }): MetricWithDims<{
        DatastoreName: string;
    }>;
    static pipelineConcurrentExecutionCountSum(this: void, dimensions: {
        PipelineActivityName: string;
    }): MetricWithDims<{
        PipelineActivityName: string;
    }>;
}
