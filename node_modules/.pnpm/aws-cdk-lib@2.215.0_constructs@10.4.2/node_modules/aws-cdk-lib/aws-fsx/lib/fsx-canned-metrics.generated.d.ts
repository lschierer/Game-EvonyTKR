export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class FSxMetrics {
    static dataReadBytesSum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static dataWriteBytesSum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static freeStorageCapacityAverage(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static freeDataStorageCapacitySum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static dataReadOperationsSum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static dataWriteOperationsSum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
    static metadataOperationsSum(this: void, dimensions: {
        FileSystemId: string;
    }): MetricWithDims<{
        FileSystemId: string;
    }>;
}
