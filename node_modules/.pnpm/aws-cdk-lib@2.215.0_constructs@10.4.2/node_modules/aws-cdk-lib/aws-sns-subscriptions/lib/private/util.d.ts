import * as sns from '../../../aws-sns';
import { IResource } from '../../../core';
export declare function regionFromArn(topic: sns.ITopic, resource: IResource): string | undefined;
