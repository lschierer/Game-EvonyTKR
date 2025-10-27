export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class RedshiftMetrics {
    static commitQueueLengthAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static concurrencyScalingActiveClustersAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static concurrencyScalingSecondsAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static cpuUtilizationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static cpuUtilizationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static databaseConnectionsAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static healthStatusSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static maintenanceModeSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static maxConfiguredConcurrencyScalingClustersSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static networkReceiveThroughputSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static networkTransmitThroughputSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static numExceededSchemaQuotasAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static percentageDiskSpaceUsedAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static percentageQuotaUsedAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static queriesCompletedPerSecondSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static queryDurationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static queryRuntimeBreakdownSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static readIopsSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static readLatencyAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static readLatencyAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static readThroughputSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static storageUsedAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static totalTableCountAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static wlmQueriesCompletedPerSecondAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static wlmQueriesCompletedPerSecondAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        wlmid: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        wlmid: string;
    }>;
    static wlmQueriesCompletedPerSecondAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        QueueName: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        QueueName: string;
    }>;
    static wlmQueryDurationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static wlmQueryDurationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        wlmid: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        wlmid: string;
    }>;
    static wlmQueryDurationAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        QueueName: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        QueueName: string;
    }>;
    static wlmQueueLengthSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static writeIopsSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static writeLatencyAverage(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static writeLatencyAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static writeThroughputSum(this: void, dimensions: {
        ClusterIdentifier: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
    }>;
    static networkReceiveThroughputAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static networkTransmitThroughputAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static readIopsAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static readThroughputAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static writeIopsAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static writeThroughputAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        NodeID: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        NodeID: string;
    }>;
    static queriesCompletedPerSecondAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        wlmid: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        wlmid: string;
    }>;
    static wlmQueueWaitTimeAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        wlmid: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        wlmid: string;
    }>;
    static wlmQueueWaitTimeAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        QueueName: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        QueueName: string;
    }>;
    static wlmRunningQueriesAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        wlmid: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        wlmid: string;
    }>;
    static wlmRunningQueriesAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        QueueName: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        QueueName: string;
    }>;
    static wlmQueueLengthAverage(this: void, dimensions: {
        ClusterIdentifier: string;
        QueueName: string;
    }): MetricWithDims<{
        ClusterIdentifier: string;
        QueueName: string;
    }>;
}
