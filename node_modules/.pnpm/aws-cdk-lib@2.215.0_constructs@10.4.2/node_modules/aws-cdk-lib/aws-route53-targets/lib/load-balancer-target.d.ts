import { IAliasRecordTargetProps } from './shared';
import * as elbv2 from '../../aws-elasticloadbalancingv2';
import * as route53 from '../../aws-route53';
/**
 * Use an ELBv2 as an alias record target
 */
export declare class LoadBalancerTarget implements route53.IAliasRecordTarget {
    private readonly loadBalancer;
    private readonly props?;
    constructor(loadBalancer: elbv2.ILoadBalancerV2, props?: IAliasRecordTargetProps | undefined);
    bind(_record: route53.IRecordSet, _zone?: route53.IHostedZone): route53.AliasRecordTargetConfig;
}
