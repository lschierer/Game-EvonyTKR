export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class GlobalAcceleratorMetrics {
    static newFlowCountSum(this: void, dimensions: {
        Accelerator: string;
    }): MetricWithDims<{
        Accelerator: string;
    }>;
    static processedBytesInSum(this: void, dimensions: {
        Accelerator: string;
    }): MetricWithDims<{
        Accelerator: string;
    }>;
    static processedBytesOutSum(this: void, dimensions: {
        Accelerator: string;
    }): MetricWithDims<{
        Accelerator: string;
    }>;
}
