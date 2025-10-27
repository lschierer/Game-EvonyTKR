export interface MetricWithDims<D> {
    readonly namespace: string;
    readonly metricName: string;
    readonly statistic: string;
    readonly dimensionsMap: D;
}
export declare class SESMetrics {
    static bounceSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static bounceSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static clickSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static clickSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static complaintSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static complaintSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static deliverySum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static deliverySum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static openSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static openSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static rejectSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static rejectSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static renderingFailureSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static renderingFailureSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static reputationBounceRateAverage(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static reputationComplaintRateAverage(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static sendSum(this: void, dimensions: {
        RuleName: string;
    }): MetricWithDims<{
        RuleName: string;
    }>;
    static sendSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static reputationBounceRateSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
    static reputationComplaintRateSum(this: void, dimensions: {
        "ses:configuration-set": string;
    }): MetricWithDims<{
        "ses:configuration-set": string;
    }>;
}
