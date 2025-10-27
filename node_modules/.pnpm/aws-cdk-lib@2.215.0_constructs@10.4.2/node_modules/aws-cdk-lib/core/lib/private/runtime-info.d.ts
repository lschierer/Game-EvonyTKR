import { IConstruct, MetadataEntry } from 'constructs';
import { Stack } from '../stack';
/**
 * Source information on a construct (class fqn and version)
 */
export interface ConstructInfo {
    readonly fqn: string;
    readonly version: string;
    readonly metadata?: Record<string, any>[];
}
export declare function constructInfoFromConstruct(construct: IConstruct): ConstructInfo | undefined;
/**
 * Filter for Construct, Method, and Feature flag metadata. Redact values from it.
 *
 * @param metadata a list of metadata entries
 */
export declare function filterMetadataType(metadata: MetadataEntry[]): Record<string, any>[];
/**
 * For a given stack, walks the tree and finds the runtime info for all constructs within the tree.
 * Returns the unique list of construct info present in the stack,
 * as long as the construct fully-qualified names match the defined allow list.
 */
export declare function constructInfoFromStack(stack: Stack): ConstructInfo[];
