import { IConstruct } from 'constructs';
import { INetworkAclRef, ISubnetRef } from '../ec2.generated';
import { INetworkAcl } from '../network-acl';
import { ISubnet } from '../vpc';
export declare function asNetworkAcl(x: INetworkAclRef, scope: IConstruct): INetworkAcl;
export declare function asSubnet(x: ISubnetRef, scope: IConstruct): ISubnet;
