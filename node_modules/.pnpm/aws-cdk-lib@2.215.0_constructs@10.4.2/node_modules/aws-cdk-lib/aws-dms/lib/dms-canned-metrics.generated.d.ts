export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class DMSMetrics {
    static cdcLatencyTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcLatencySourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static availableMemoryAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcChangesDiskTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcChangesMemorySourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcChangesMemoryTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcIncomingChangesSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcThroughputBandwidthSourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcThroughputBandwidthTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcThroughputRowsSourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cdcThroughputRowsTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cpuAllocatedSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static cpuUtilizationAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static freeMemoryAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static fullLoadThroughputBandwidthSourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static fullLoadThroughputBandwidthTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static fullLoadThroughputRowsSourceSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static fullLoadThroughputRowsTargetSum(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static memoryAllocatedAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static memoryUsageAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
    static swapUsageAverage(this: void, dimensions: {
        ReplicationInstanceIdentifier: string;
    }): MetricWithDims<{
        ReplicationInstanceIdentifier: string;
    }>;
}
