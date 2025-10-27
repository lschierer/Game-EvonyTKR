export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class LogsMetrics {
    static incomingLogEventsSum(this: void, dimensions: {
        LogGroupName: string;
    }): MetricWithDims<{
        LogGroupName: string;
    }>;
    static incomingBytesSum(this: void, dimensions: {
        LogGroupName: string;
    }): MetricWithDims<{
        LogGroupName: string;
    }>;
}
