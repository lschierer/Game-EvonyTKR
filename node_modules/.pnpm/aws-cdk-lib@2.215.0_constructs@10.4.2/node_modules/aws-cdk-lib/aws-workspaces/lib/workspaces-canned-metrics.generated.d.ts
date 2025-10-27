export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class WorkSpacesMetrics {
    static availableAverage(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static unhealthyAverage(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static sessionLaunchTimeAverage(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static connectionSuccessSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static connectionFailureSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static connectionAttemptSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static inSessionLatencyAverage(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static sessionDisconnectSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static userConnectedSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static stoppedSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
    static maintenanceSum(this: void, dimensions: {
        WorkspaceId: string;
    }): MetricWithDims<{
        WorkspaceId: string;
    }>;
}
