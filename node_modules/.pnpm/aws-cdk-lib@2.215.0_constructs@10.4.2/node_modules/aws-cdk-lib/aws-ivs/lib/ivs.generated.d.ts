import * as cdk from "../../core";
import * as constructs from "constructs";
import * as cfn_parse from "../../core/lib/helpers-internal";
/**
 * Indicates that this resource can be referenced as a Channel.
 *
 * @stability experimental
 */
export interface IChannelRef extends constructs.IConstruct {
    /**
     * A reference to a Channel resource.
     */
    readonly channelRef: ChannelReference;
}
/**
 * The `AWS::IVS::Channel` resource specifies an  channel.
 *
 * A channel stores configuration information related to your live stream. For more information, see [CreateChannel](https://docs.aws.amazon.com/ivs/latest/LowLatencyAPIReference/API_CreateChannel.html) in the *Amazon IVS Low-Latency Streaming API Reference* .
 *
 * > By default, the IVS API CreateChannel endpoint creates a stream key in addition to a channel. The  Channel resource *does not* create a stream key; to create a stream key, use the StreamKey resource instead.
 *
 * @cloudformationResource AWS::IVS::Channel
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html
 */
export declare class CfnChannel extends cdk.CfnResource implements cdk.IInspectable, IChannelRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnChannel from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnChannel;
    /**
     * Channel ARN is automatically generated on creation and assigned as the unique identifier.
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Channel ingest endpoint, part of the definition of an ingest server, used when you set up streaming software.
     *
     * @cloudformationAttribute IngestEndpoint
     */
    readonly attrIngestEndpoint: string;
    /**
     * Channel Playback URL.
     *
     * @cloudformationAttribute PlaybackUrl
     */
    readonly attrPlaybackUrl: string;
    /**
     * Whether the channel is authorized.
     */
    authorized?: boolean | cdk.IResolvable;
    /**
     * Indicates which content-packaging format is used (MPEG-TS or fMP4).
     */
    containerFormat?: string;
    /**
     * Whether the channel allows insecure ingest.
     */
    insecureIngest?: boolean | cdk.IResolvable;
    /**
     * Channel latency mode.
     */
    latencyMode?: string;
    /**
     * Object specifying multitrack input configuration.
     */
    multitrackInputConfiguration?: cdk.IResolvable | CfnChannel.MultitrackInputConfigurationProperty;
    /**
     * Channel.
     */
    name?: string;
    /**
     * Optional transcode preset for the channel.
     */
    preset?: string;
    /**
     * Recording Configuration ARN.
     */
    recordingConfigurationArn?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * A list of key-value pairs that contain metadata for the asset model.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * Channel type, which determines the allowable resolution and bitrate.
     */
    type?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnChannelProps);
    get channelRef(): ChannelReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnChannel {
    /**
     * A complex type that specifies multitrack input configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-channel-multitrackinputconfiguration.html
     */
    interface MultitrackInputConfigurationProperty {
        /**
         * Indicates whether multitrack input is enabled.
         *
         * Can be set to true only if channel type is STANDARD. Setting enabled to true with any other channel type will cause an exception. If true, then policy, maximumResolution, and containerFormat are required, and containerFormat must be set to FRAGMENTED_MP4. Default: false.
         *
         * @default - false
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-channel-multitrackinputconfiguration.html#cfn-ivs-channel-multitrackinputconfiguration-enabled
         */
        readonly enabled?: boolean | cdk.IResolvable;
        /**
         * Maximum resolution for multitrack input.
         *
         * Required if enabled is true.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-channel-multitrackinputconfiguration.html#cfn-ivs-channel-multitrackinputconfiguration-maximumresolution
         */
        readonly maximumResolution?: string;
        /**
         * Indicates whether multitrack input is allowed or required.
         *
         * Required if enabled is true.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-channel-multitrackinputconfiguration.html#cfn-ivs-channel-multitrackinputconfiguration-policy
         */
        readonly policy?: string;
    }
}
/**
 * Properties for defining a `CfnChannel`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html
 */
export interface CfnChannelProps {
    /**
     * Whether the channel is authorized.
     *
     * @default - false
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-authorized
     */
    readonly authorized?: boolean | cdk.IResolvable;
    /**
     * Indicates which content-packaging format is used (MPEG-TS or fMP4).
     *
     * If multitrackInputConfiguration is specified and enabled is true, then containerFormat is required and must be set to FRAGMENTED_MP4. Otherwise, containerFormat may be set to TS or FRAGMENTED_MP4. Default: TS.
     *
     * @default - "TS"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-containerformat
     */
    readonly containerFormat?: string;
    /**
     * Whether the channel allows insecure ingest.
     *
     * @default - false
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-insecureingest
     */
    readonly insecureIngest?: boolean | cdk.IResolvable;
    /**
     * Channel latency mode.
     *
     * @default - "LOW"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-latencymode
     */
    readonly latencyMode?: string;
    /**
     * Object specifying multitrack input configuration.
     *
     * Default: no multitrack input configuration is specified.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-multitrackinputconfiguration
     */
    readonly multitrackInputConfiguration?: cdk.IResolvable | CfnChannel.MultitrackInputConfigurationProperty;
    /**
     * Channel.
     *
     * @default - "-"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-name
     */
    readonly name?: string;
    /**
     * Optional transcode preset for the channel.
     *
     * This is selectable only for ADVANCED_HD and ADVANCED_SD channel types. For those channel types, the default preset is HIGHER_BANDWIDTH_DELIVERY. For other channel types (BASIC and STANDARD), preset is the empty string ("").
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-preset
     */
    readonly preset?: string;
    /**
     * Recording Configuration ARN.
     *
     * A value other than an empty string indicates that recording is enabled. Default: "" (recording is disabled).
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-recordingconfigurationarn
     */
    readonly recordingConfigurationArn?: string;
    /**
     * A list of key-value pairs that contain metadata for the asset model.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * Channel type, which determines the allowable resolution and bitrate.
     *
     * If you exceed the allowable resolution or bitrate, the stream probably will disconnect immediately.
     *
     * @default - "STANDARD"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-channel.html#cfn-ivs-channel-type
     */
    readonly type?: string;
}
/**
 * A reference to a Channel resource.
 *
 * @struct
 * @stability external
 */
export interface ChannelReference {
    /**
     * The Arn of the Channel resource.
     */
    readonly channelArn: string;
}
/**
 * Indicates that this resource can be referenced as a PlaybackKeyPair.
 *
 * @stability experimental
 */
export interface IPlaybackKeyPairRef extends constructs.IConstruct {
    /**
     * A reference to a PlaybackKeyPair resource.
     */
    readonly playbackKeyPairRef: PlaybackKeyPairReference;
}
/**
 * The `AWS::IVS::PlaybackKeyPair` resource specifies an  playback key pair.
 *
 * uses a public playback key to validate playback tokens that have been signed with the corresponding private key. For more information, see [Setting Up Private Channels](https://docs.aws.amazon.com/ivs/latest/LowLatencyUserGuide/private-channels.html) in the *Amazon IVS Low-Latency Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::PlaybackKeyPair
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackkeypair.html
 */
export declare class CfnPlaybackKeyPair extends cdk.CfnResource implements cdk.IInspectable, IPlaybackKeyPairRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnPlaybackKeyPair from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnPlaybackKeyPair;
    /**
     * Key-pair ARN. For example: `arn:aws:ivs:us-west-2:693991300569:playback-key/f99cde61-c2b0-4df3-8941-ca7d38acca1a`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Key-pair identifier. For example: `98:0d:1a:a0:19:96:1e:ea:0a:0a:2c:9a:42:19:2b:e7`
     *
     * @cloudformationAttribute Fingerprint
     */
    readonly attrFingerprint: string;
    /**
     * Playback-key-pair name.
     */
    name?: string;
    /**
     * The public portion of a customer-generated key pair.
     */
    publicKeyMaterial?: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnPlaybackKeyPairProps);
    get playbackKeyPairRef(): PlaybackKeyPairReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnPlaybackKeyPair`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackkeypair.html
 */
export interface CfnPlaybackKeyPairProps {
    /**
     * Playback-key-pair name.
     *
     * The value does not need to be unique.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackkeypair.html#cfn-ivs-playbackkeypair-name
     */
    readonly name?: string;
    /**
     * The public portion of a customer-generated key pair.
     *
     * Note that this field is required to create the AWS::IVS::PlaybackKeyPair resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackkeypair.html#cfn-ivs-playbackkeypair-publickeymaterial
     */
    readonly publicKeyMaterial?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-playbackkeypair-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackkeypair.html#cfn-ivs-playbackkeypair-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a PlaybackKeyPair resource.
 *
 * @struct
 * @stability external
 */
export interface PlaybackKeyPairReference {
    /**
     * The Arn of the PlaybackKeyPair resource.
     */
    readonly playbackKeyPairArn: string;
}
/**
 * Indicates that this resource can be referenced as a RecordingConfiguration.
 *
 * @stability experimental
 */
export interface IRecordingConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a RecordingConfiguration resource.
     */
    readonly recordingConfigurationRef: RecordingConfigurationReference;
}
/**
 * The `AWS::IVS::RecordingConfiguration` resource specifies an  recording configuration.
 *
 * A recording configuration enables the recording of a channel’s live streams to a data store. Multiple channels can reference the same recording configuration. For more information, see [RecordingConfiguration](https://docs.aws.amazon.com/ivs/latest/LowLatencyAPIReference/API_RecordingConfiguration.html) in the *Amazon IVS Low-Latency Streaming API Reference* .
 *
 * @cloudformationResource AWS::IVS::RecordingConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html
 */
export declare class CfnRecordingConfiguration extends cdk.CfnResource implements cdk.IInspectable, IRecordingConfigurationRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnRecordingConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnRecordingConfiguration;
    /**
     * The recording configuration ARN. For example: `arn:aws:ivs:us-west-2:123456789012:recording-configuration/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Indicates the current state of the recording configuration. When the state is `ACTIVE` , the configuration is ready to record a channel stream. Valid values: `CREATING` | `CREATE_FAILED` | `ACTIVE` .
     *
     * @cloudformationAttribute State
     */
    readonly attrState: string;
    /**
     * A destination configuration describes an S3 bucket where recorded video will be stored.
     */
    destinationConfiguration: CfnRecordingConfiguration.DestinationConfigurationProperty | cdk.IResolvable;
    /**
     * Recording-configuration name.
     */
    name?: string;
    /**
     * If a broadcast disconnects and then reconnects within the specified interval, the multiple streams will be considered a single broadcast and merged together.
     */
    recordingReconnectWindowSeconds?: number;
    /**
     * A rendition configuration describes which renditions should be recorded for a stream.
     */
    renditionConfiguration?: cdk.IResolvable | CfnRecordingConfiguration.RenditionConfigurationProperty;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * A thumbnail configuration enables/disables the recording of thumbnails for a live session and controls the interval at which thumbnails are generated for the live session.
     */
    thumbnailConfiguration?: cdk.IResolvable | CfnRecordingConfiguration.ThumbnailConfigurationProperty;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnRecordingConfigurationProps);
    get recordingConfigurationRef(): RecordingConfigurationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnRecordingConfiguration {
    /**
     * The DestinationConfiguration property type describes the location where recorded videos will be stored.
     *
     * Each member represents a type of destination configuration. For recording, you define one and only one type of destination configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-destinationconfiguration.html
     */
    interface DestinationConfigurationProperty {
        /**
         * An S3 destination configuration where recorded videos will be stored.
         *
         * See the [S3DestinationConfiguration](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-s3destinationconfiguration.html) property type for more information.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-destinationconfiguration.html#cfn-ivs-recordingconfiguration-destinationconfiguration-s3
         */
        readonly s3?: cdk.IResolvable | CfnRecordingConfiguration.S3DestinationConfigurationProperty;
    }
    /**
     * The S3DestinationConfiguration property type describes an S3 location where recorded videos will be stored.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-s3destinationconfiguration.html
     */
    interface S3DestinationConfigurationProperty {
        /**
         * Location (S3 bucket name) where recorded videos will be stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-s3destinationconfiguration.html#cfn-ivs-recordingconfiguration-s3destinationconfiguration-bucketname
         */
        readonly bucketName: string;
    }
    /**
     * The RenditionConfiguration property type describes which renditions should be recorded for a stream.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-renditionconfiguration.html
     */
    interface RenditionConfigurationProperty {
        /**
         * A list of which renditions are recorded for a stream, if `renditionSelection` is `CUSTOM` ;
         *
         * otherwise, this field is irrelevant. The selected renditions are recorded if they are available during the stream. If a selected rendition is unavailable, the best available rendition is recorded. For details on the resolution dimensions of each rendition, see [Auto-Record to Amazon S3](https://docs.aws.amazon.com//ivs/latest/LowLatencyUserGuide/record-to-s3.html) .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-renditionconfiguration.html#cfn-ivs-recordingconfiguration-renditionconfiguration-renditions
         */
        readonly renditions?: Array<string>;
        /**
         * The set of renditions are recorded for a stream.
         *
         * For `BASIC` channels, the `CUSTOM` value has no effect. If `CUSTOM` is specified, a set of renditions can be specified in the `renditions` field. Default: `ALL` .
         *
         * @default - "ALL"
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-renditionconfiguration.html#cfn-ivs-recordingconfiguration-renditionconfiguration-renditionselection
         */
        readonly renditionSelection?: string;
    }
    /**
     * The ThumbnailConfiguration property type describes a configuration of thumbnails for recorded video.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html
     */
    interface ThumbnailConfigurationProperty {
        /**
         * Thumbnail recording mode. Valid values:.
         *
         * - `DISABLED` : Use DISABLED to disable the generation of thumbnails for recorded video.
         * - `INTERVAL` : Use INTERVAL to enable the generation of thumbnails for recorded video at a time interval controlled by the [TargetIntervalSeconds](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-targetintervalseconds) property.
         *
         * *Default* : `INTERVAL`
         *
         * @default - "INTERVAL"
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-recordingmode
         */
        readonly recordingMode?: string;
        /**
         * The desired resolution of recorded thumbnails for a stream.
         *
         * Thumbnails are recorded at the selected resolution if the corresponding rendition is available during the stream; otherwise, they are recorded at source resolution. For more information about resolution values and their corresponding height and width dimensions, see [Auto-Record to Amazon S3](https://docs.aws.amazon.com//ivs/latest/LowLatencyUserGuide/record-to-s3.html) .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-resolution
         */
        readonly resolution?: string;
        /**
         * The format in which thumbnails are recorded for a stream.
         *
         * `SEQUENTIAL` records all generated thumbnails in a serial manner, to the media/thumbnails directory. `LATEST` saves the latest thumbnail in media/thumbnails/latest/thumb.jpg and overwrites it at the interval specified by `targetIntervalSeconds` . You can enable both `SEQUENTIAL` and `LATEST` . Default: `SEQUENTIAL` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-storage
         */
        readonly storage?: Array<string>;
        /**
         * The targeted thumbnail-generation interval in seconds. This is configurable (and required) only if [RecordingMode](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-recordingmode) is `INTERVAL` .
         *
         * > Setting a value for `TargetIntervalSeconds` does not guarantee that thumbnails are generated at the specified interval. For thumbnails to be generated at the `TargetIntervalSeconds` interval, the `IDR/Keyframe` value for the input video must be less than the `TargetIntervalSeconds` value. See [Amazon IVS Streaming Configuration](https://docs.aws.amazon.com/ivs/latest/LowLatencyUserGuide/streaming-config.html) for information on setting `IDR/Keyframe` to the recommended value in video-encoder settings.
         *
         * *Default* : 60
         *
         * @default - 60
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-thumbnailconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration-targetintervalseconds
         */
        readonly targetIntervalSeconds?: number;
    }
}
/**
 * Properties for defining a `CfnRecordingConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html
 */
export interface CfnRecordingConfigurationProps {
    /**
     * A destination configuration describes an S3 bucket where recorded video will be stored.
     *
     * See the DestinationConfiguration property type for more information.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-destinationconfiguration
     */
    readonly destinationConfiguration: CfnRecordingConfiguration.DestinationConfigurationProperty | cdk.IResolvable;
    /**
     * Recording-configuration name.
     *
     * The value does not need to be unique.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-name
     */
    readonly name?: string;
    /**
     * If a broadcast disconnects and then reconnects within the specified interval, the multiple streams will be considered a single broadcast and merged together.
     *
     * *Default* : `0`
     *
     * @default - 0
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-recordingreconnectwindowseconds
     */
    readonly recordingReconnectWindowSeconds?: number;
    /**
     * A rendition configuration describes which renditions should be recorded for a stream.
     *
     * See the RenditionConfiguration property type for more information.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-renditionconfiguration
     */
    readonly renditionConfiguration?: cdk.IResolvable | CfnRecordingConfiguration.RenditionConfigurationProperty;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-recordingconfiguration-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * A thumbnail configuration enables/disables the recording of thumbnails for a live session and controls the interval at which thumbnails are generated for the live session.
     *
     * See the ThumbnailConfiguration property type for more information.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-recordingconfiguration.html#cfn-ivs-recordingconfiguration-thumbnailconfiguration
     */
    readonly thumbnailConfiguration?: cdk.IResolvable | CfnRecordingConfiguration.ThumbnailConfigurationProperty;
}
/**
 * A reference to a RecordingConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface RecordingConfigurationReference {
    /**
     * The Arn of the RecordingConfiguration resource.
     */
    readonly recordingConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a StreamKey.
 *
 * @stability experimental
 */
export interface IStreamKeyRef extends constructs.IConstruct {
    /**
     * A reference to a StreamKey resource.
     */
    readonly streamKeyRef: StreamKeyReference;
}
/**
 * The `AWS::IVS::StreamKey` resource specifies an  stream key associated with the referenced channel.
 *
 * Use a stream key to initiate a live stream.
 *
 * @cloudformationResource AWS::IVS::StreamKey
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-streamkey.html
 */
export declare class CfnStreamKey extends cdk.CfnResource implements cdk.IInspectable, IStreamKeyRef, cdk.ITaggable {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnStreamKey from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnStreamKey;
    /**
     * The stream-key ARN. For example: `arn:aws:ivs:us-west-2:123456789012:stream-key/g1H2I3j4k5L6`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The stream-key value. For example: `sk_us-west-2_abcdABCDefgh_567890abcdef`
     *
     * @cloudformationAttribute Value
     */
    readonly attrValue: string;
    /**
     * Channel ARN for the stream.
     */
    channelArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly tags: cdk.TagManager;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tagsRaw?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnStreamKeyProps);
    get streamKeyRef(): StreamKeyReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnStreamKey`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-streamkey.html
 */
export interface CfnStreamKeyProps {
    /**
     * Channel ARN for the stream.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-streamkey.html#cfn-ivs-streamkey-channelarn
     */
    readonly channelArn: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-streamkey-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-streamkey.html#cfn-ivs-streamkey-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a StreamKey resource.
 *
 * @struct
 * @stability external
 */
export interface StreamKeyReference {
    /**
     * The Arn of the StreamKey resource.
     */
    readonly streamKeyArn: string;
}
/**
 * Indicates that this resource can be referenced as a EncoderConfiguration.
 *
 * @stability experimental
 */
export interface IEncoderConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a EncoderConfiguration resource.
     */
    readonly encoderConfigurationRef: EncoderConfigurationReference;
}
/**
 * The `AWS::IVS::EncoderConfiguration` resource specifies an  encoder configuration.
 *
 * An encoder configuration describes a stream’s video configuration. For more information, see [Streaming Configuration](https://docs.aws.amazon.com/ivs/latest/LowLatencyUserGuide/streaming-config.html) in the *Amazon IVS Low-Latency Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::EncoderConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-encoderconfiguration.html
 */
export declare class CfnEncoderConfiguration extends cdk.CfnResource implements cdk.IInspectable, IEncoderConfigurationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnEncoderConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnEncoderConfiguration;
    /**
     * The encoder-configuration ARN. For example: `arn:aws:ivs:us-west-2:123456789012:encoder-configuration/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Encoder cnfiguration name.
     */
    name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * Video configuration.
     */
    video?: cdk.IResolvable | CfnEncoderConfiguration.VideoProperty;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnEncoderConfigurationProps);
    get encoderConfigurationRef(): EncoderConfigurationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnEncoderConfiguration {
    /**
     * The Video property type describes a stream's video configuration.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html
     */
    interface VideoProperty {
        /**
         * Bitrate for generated output, in bps.
         *
         * Default: 2500000.
         *
         * @default - 2500000
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html#cfn-ivs-encoderconfiguration-video-bitrate
         */
        readonly bitrate?: number;
        /**
         * Video frame rate, in fps.
         *
         * Default: 30.
         *
         * @default - 30
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html#cfn-ivs-encoderconfiguration-video-framerate
         */
        readonly framerate?: number;
        /**
         * Video-resolution height.
         *
         * Note that the maximum value is determined by width times height, such that the maximum total pixels is 2073600 (1920x1080 or 1080x1920). Default: 720.
         *
         * @default - 720
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html#cfn-ivs-encoderconfiguration-video-height
         */
        readonly height?: number;
        /**
         * Video-resolution width.
         *
         * Note that the maximum value is determined by width times height, such that the maximum total pixels is 2073600 (1920x1080 or 1080x1920). Default: 1280.
         *
         * @default - 1280
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html#cfn-ivs-encoderconfiguration-video-width
         */
        readonly width?: number;
    }
}
/**
 * Properties for defining a `CfnEncoderConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-encoderconfiguration.html
 */
export interface CfnEncoderConfigurationProps {
    /**
     * Encoder cnfiguration name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-encoderconfiguration.html#cfn-ivs-encoderconfiguration-name
     */
    readonly name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-encoderconfiguration.html#cfn-ivs-encoderconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * Video configuration.
     *
     * Default: video resolution 1280x720, bitrate 2500 kbps, 30 fps. See the [Video](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-encoderconfiguration-video.html) property type for more information.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-encoderconfiguration.html#cfn-ivs-encoderconfiguration-video
     */
    readonly video?: cdk.IResolvable | CfnEncoderConfiguration.VideoProperty;
}
/**
 * A reference to a EncoderConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface EncoderConfigurationReference {
    /**
     * The Arn of the EncoderConfiguration resource.
     */
    readonly encoderConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a IngestConfiguration.
 *
 * @stability experimental
 */
export interface IIngestConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a IngestConfiguration resource.
     */
    readonly ingestConfigurationRef: IngestConfigurationReference;
}
/**
 * The `AWS::IVS::IngestConfiguration` resource specifies an ingest protocol to be used for a stage.
 *
 * For more information, see [Stream Ingest](https://docs.aws.amazon.com/ivs/latest/RealTimeUserGuide/rt-stream-ingest.html) in the *Amazon IVS Real-Time Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::IngestConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html
 */
export declare class CfnIngestConfiguration extends cdk.CfnResource implements cdk.IInspectable, IIngestConfigurationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnIngestConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnIngestConfiguration;
    /**
     * The ingest-configuration ARN. For example: `arn:aws:ivs:us-west-2:123456789012:ingest-configuration/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * ID of the participant within the stage. For example: `abCDEf12GHIj`
     *
     * @cloudformationAttribute ParticipantId
     */
    readonly attrParticipantId: string;
    /**
     * State of the ingest configuration. It is `ACTIVE` if a publisher currently is publishing to the stage associated with the ingest configuration. Valid values: `ACTIVE` | `INACTIVE` .
     *
     * @cloudformationAttribute State
     */
    readonly attrState: string;
    /**
     * Ingest-key value for the RTMP(S) protocol. For example: `skSKABCDefgh`
     *
     * @cloudformationAttribute StreamKey
     */
    readonly attrStreamKey: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Type of ingest protocol that the user employs for broadcasting.
     */
    ingestProtocol?: string;
    /**
     * Whether the channel allows insecure RTMP ingest.
     */
    insecureIngest?: boolean | cdk.IResolvable;
    /**
     * Ingest name.
     */
    name?: string;
    /**
     * ARN of the stage with which the IngestConfiguration is associated.
     */
    stageArn?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * Customer-assigned name to help identify the participant using the IngestConfiguration;
     */
    userId?: string;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnIngestConfigurationProps);
    get ingestConfigurationRef(): IngestConfigurationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnIngestConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html
 */
export interface CfnIngestConfigurationProps {
    /**
     * Type of ingest protocol that the user employs for broadcasting.
     *
     * @default - "RTMPS"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-ingestprotocol
     */
    readonly ingestProtocol?: string;
    /**
     * Whether the channel allows insecure RTMP ingest.
     *
     * Default: `false` .
     *
     * @default - false
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-insecureingest
     */
    readonly insecureIngest?: boolean | cdk.IResolvable;
    /**
     * Ingest name.
     *
     * @default - "-"
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-name
     */
    readonly name?: string;
    /**
     * ARN of the stage with which the IngestConfiguration is associated.
     *
     * @default - ""
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-stagearn
     */
    readonly stageArn?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
    /**
     * Customer-assigned name to help identify the participant using the IngestConfiguration;
     *
     * this can be used to link a participant to a user in the customer’s own systems. This can be any UTF-8 encoded text. *This field is exposed to all stage participants and should not be used for personally identifying, confidential, or sensitive information.*
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-ingestconfiguration.html#cfn-ivs-ingestconfiguration-userid
     */
    readonly userId?: string;
}
/**
 * A reference to a IngestConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface IngestConfigurationReference {
    /**
     * The Arn of the IngestConfiguration resource.
     */
    readonly ingestConfigurationArn: string;
}
/**
 * Indicates that this resource can be referenced as a PlaybackRestrictionPolicy.
 *
 * @stability experimental
 */
export interface IPlaybackRestrictionPolicyRef extends constructs.IConstruct {
    /**
     * A reference to a PlaybackRestrictionPolicy resource.
     */
    readonly playbackRestrictionPolicyRef: PlaybackRestrictionPolicyReference;
}
/**
 * The `AWS::IVS::PlaybackRestrictionPolicy` resource specifies an  playback restriction policy.
 *
 * A playback restriction policy constrains playback by country and/or origin sites. For more information, see [Undesired Content and Viewers](https://docs.aws.amazon.com/ivs/latest/LowLatencyUserGuide/undesired-content.html) in the *Amazon IVS Low-Latency Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::PlaybackRestrictionPolicy
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html
 */
export declare class CfnPlaybackRestrictionPolicy extends cdk.CfnResource implements cdk.IInspectable, IPlaybackRestrictionPolicyRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnPlaybackRestrictionPolicy from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnPlaybackRestrictionPolicy;
    /**
     * The playback-restriction-policy ARN. For example: `arn:aws:ivs:us-west-2:123456789012:playback-restriction-policy/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * A list of country codes that control geoblocking restrictions.
     */
    allowedCountries?: Array<string>;
    /**
     * A list of origin sites that control CORS restriction.
     */
    allowedOrigins?: Array<string>;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Whether channel playback is constrained by the origin site.
     */
    enableStrictOriginEnforcement?: boolean | cdk.IResolvable;
    /**
     * Playback-restriction-policy name.
     */
    name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnPlaybackRestrictionPolicyProps);
    get playbackRestrictionPolicyRef(): PlaybackRestrictionPolicyReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnPlaybackRestrictionPolicy`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html
 */
export interface CfnPlaybackRestrictionPolicyProps {
    /**
     * A list of country codes that control geoblocking restrictions.
     *
     * Allowed values are the officially assigned ISO 3166-1 alpha-2 codes. Default: All countries (an empty array).
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html#cfn-ivs-playbackrestrictionpolicy-allowedcountries
     */
    readonly allowedCountries?: Array<string>;
    /**
     * A list of origin sites that control CORS restriction.
     *
     * Allowed values are the same as valid values of the Origin header defined at [https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Origin"](https://docs.aws.amazon.com/https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Origin)
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html#cfn-ivs-playbackrestrictionpolicy-allowedorigins
     */
    readonly allowedOrigins?: Array<string>;
    /**
     * Whether channel playback is constrained by the origin site.
     *
     * @default - false
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html#cfn-ivs-playbackrestrictionpolicy-enablestrictoriginenforcement
     */
    readonly enableStrictOriginEnforcement?: boolean | cdk.IResolvable;
    /**
     * Playback-restriction-policy name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html#cfn-ivs-playbackrestrictionpolicy-name
     */
    readonly name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-playbackrestrictionpolicy-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-playbackrestrictionpolicy.html#cfn-ivs-playbackrestrictionpolicy-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a PlaybackRestrictionPolicy resource.
 *
 * @struct
 * @stability external
 */
export interface PlaybackRestrictionPolicyReference {
    /**
     * The Arn of the PlaybackRestrictionPolicy resource.
     */
    readonly playbackRestrictionPolicyArn: string;
}
/**
 * Indicates that this resource can be referenced as a PublicKey.
 *
 * @stability experimental
 */
export interface IPublicKeyRef extends constructs.IConstruct {
    /**
     * A reference to a PublicKey resource.
     */
    readonly publicKeyRef: PublicKeyReference;
}
/**
 * The `AWS::IVS::PublicKey` resource specifies an Amazon IVS public key used to sign stage participant tokens.
 *
 * For more information, see [Distribute Participant Tokens](https://docs.aws.amazon.com/ivs/latest/RealTimeUserGuide/getting-started-distribute-tokens.html) in the *Amazon IVS Real-Time Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::PublicKey
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-publickey.html
 */
export declare class CfnPublicKey extends cdk.CfnResource implements cdk.IInspectable, IPublicKeyRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnPublicKey from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnPublicKey;
    /**
     * The public key ARN. For example: `arn:aws:ivs:us-west-2:123456789012:public-key/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * The public key identifier. For example: `98:0d:1a:a0:19:96:1e:ea:0a:0a:2c:9a:42:19:2b:e7`
     *
     * @cloudformationAttribute Fingerprint
     */
    readonly attrFingerprint: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Public key name.
     */
    name?: string;
    /**
     * The public portion of a customer-generated key pair.
     */
    publicKeyMaterial?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnPublicKeyProps);
    get publicKeyRef(): PublicKeyReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
/**
 * Properties for defining a `CfnPublicKey`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-publickey.html
 */
export interface CfnPublicKeyProps {
    /**
     * Public key name.
     *
     * The value does not need to be unique.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-publickey.html#cfn-ivs-publickey-name
     */
    readonly name?: string;
    /**
     * The public portion of a customer-generated key pair.
     *
     * Note that this field is required to create the AWS::IVS::PublicKey resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-publickey.html#cfn-ivs-publickey-publickeymaterial
     */
    readonly publicKeyMaterial?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-publickey.html#cfn-ivs-publickey-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a PublicKey resource.
 *
 * @struct
 * @stability external
 */
export interface PublicKeyReference {
    /**
     * The Arn of the PublicKey resource.
     */
    readonly publicKeyArn: string;
}
/**
 * Indicates that this resource can be referenced as a Stage.
 *
 * @stability experimental
 */
export interface IStageRef extends constructs.IConstruct {
    /**
     * A reference to a Stage resource.
     */
    readonly stageRef: StageReference;
}
/**
 * The `AWS::IVS::Stage` resource specifies an  stage.
 *
 * A stage is a virtual space where participants can exchange video in real time. For more information, see [CreateStage](https://docs.aws.amazon.com/ivs/latest/RealTimeAPIReference/API_CreateStage.html) in the *Amazon IVS Real-Time Streaming API Reference* .
 *
 * @cloudformationResource AWS::IVS::Stage
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-stage.html
 */
export declare class CfnStage extends cdk.CfnResource implements cdk.IInspectable, IStageRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnStage from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnStage;
    /**
     * ID of the active session within the stage. For example: `st-a1b2c3d4e5f6g`
     *
     * @cloudformationAttribute ActiveSessionId
     */
    readonly attrActiveSessionId: string;
    /**
     * The stage ARN. For example: `arn:aws:ivs:us-west-2:123456789012:stage/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Configuration object for individual participant recording.
     */
    autoParticipantRecordingConfiguration?: CfnStage.AutoParticipantRecordingConfigurationProperty | cdk.IResolvable;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Stage name.
     */
    name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props?: CfnStageProps);
    get stageRef(): StageReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnStage {
    /**
     * The `AWS::IVS::AutoParticipantRecordingConfiguration` property type describes a configuration for individual participant recording.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html
     */
    interface AutoParticipantRecordingConfigurationProperty {
        /**
         * HLS configuration object for individual participant recording.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html#cfn-ivs-stage-autoparticipantrecordingconfiguration-hlsconfiguration
         */
        readonly hlsConfiguration?: CfnStage.HlsConfigurationProperty | cdk.IResolvable;
        /**
         * Types of media to be recorded.
         *
         * Default: `AUDIO_VIDEO` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html#cfn-ivs-stage-autoparticipantrecordingconfiguration-mediatypes
         */
        readonly mediaTypes?: Array<string>;
        /**
         * If a stage publisher disconnects and then reconnects within the specified interval, the multiple recordings will be considered a single recording and merged together.
         *
         * The default value is 0, which disables merging.
         *
         * @default - 0
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html#cfn-ivs-stage-autoparticipantrecordingconfiguration-recordingreconnectwindowseconds
         */
        readonly recordingReconnectWindowSeconds?: number;
        /**
         * ARN of the StorageConfiguration resource to use for individual participant recording.
         *
         * Default: "" (empty string, no storage configuration is specified). Individual participant recording cannot be started unless a storage configuration is specified, when a Stage is created or updated.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html#cfn-ivs-stage-autoparticipantrecordingconfiguration-storageconfigurationarn
         */
        readonly storageConfigurationArn: string;
        /**
         * A complex type that allows you to enable/disable the recording of thumbnails for individual participant recording and modify the interval at which thumbnails are generated for the live session.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-autoparticipantrecordingconfiguration.html#cfn-ivs-stage-autoparticipantrecordingconfiguration-thumbnailconfiguration
         */
        readonly thumbnailConfiguration?: cdk.IResolvable | CfnStage.ThumbnailConfigurationProperty;
    }
    /**
     * Object specifying an HLS configuration for individual participant recording.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-hlsconfiguration.html
     */
    interface HlsConfigurationProperty {
        /**
         * Object specifying a configuration of participant HLS recordings for individual participant recording.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-hlsconfiguration.html#cfn-ivs-stage-hlsconfiguration-participantrecordinghlsconfiguration
         */
        readonly participantRecordingHlsConfiguration?: cdk.IResolvable | CfnStage.ParticipantRecordingHlsConfigurationProperty;
    }
    /**
     * Object specifying a configuration of participant HLS recordings for individual participant recording.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantrecordinghlsconfiguration.html
     */
    interface ParticipantRecordingHlsConfigurationProperty {
        /**
         * Defines the target duration for recorded segments generated when recording a stage participant.
         *
         * Segments may have durations longer than the specified value when needed to ensure each segment begins with a keyframe. Default: 6.
         *
         * @default - 6
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantrecordinghlsconfiguration.html#cfn-ivs-stage-participantrecordinghlsconfiguration-targetsegmentdurationseconds
         */
        readonly targetSegmentDurationSeconds?: number;
    }
    /**
     * An object representing a configuration of thumbnails for recorded video.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-thumbnailconfiguration.html
     */
    interface ThumbnailConfigurationProperty {
        /**
         * Object specifying a configuration of thumbnails for recorded video from an individual participant.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-thumbnailconfiguration.html#cfn-ivs-stage-thumbnailconfiguration-participantthumbnailconfiguration
         */
        readonly participantThumbnailConfiguration?: cdk.IResolvable | CfnStage.ParticipantThumbnailConfigurationProperty;
    }
    /**
     * Object specifying a configuration of thumbnails for recorded video from an individual participant.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantthumbnailconfiguration.html
     */
    interface ParticipantThumbnailConfigurationProperty {
        /**
         * Thumbnail recording mode.
         *
         * Default: `DISABLED` .
         *
         * @default - "DISABLED"
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantthumbnailconfiguration.html#cfn-ivs-stage-participantthumbnailconfiguration-recordingmode
         */
        readonly recordingMode?: string;
        /**
         * Indicates the format in which thumbnails are recorded.
         *
         * `SEQUENTIAL` records all generated thumbnails in a serial manner, to the media/thumbnails/high directory. `LATEST` saves the latest thumbnail in media/latest_thumbnail/high/thumb.jpg and overwrites it at the interval specified by `targetIntervalSeconds` . You can enable both `SEQUENTIAL` and `LATEST` . Default: `SEQUENTIAL` .
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantthumbnailconfiguration.html#cfn-ivs-stage-participantthumbnailconfiguration-storage
         */
        readonly storage?: Array<string>;
        /**
         * The targeted thumbnail-generation interval in seconds.
         *
         * This is configurable only if `recordingMode` is `INTERVAL` . Default: 60.
         *
         * @default - 60
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-participantthumbnailconfiguration.html#cfn-ivs-stage-participantthumbnailconfiguration-targetintervalseconds
         */
        readonly targetIntervalSeconds?: number;
    }
}
/**
 * Properties for defining a `CfnStage`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-stage.html
 */
export interface CfnStageProps {
    /**
     * Configuration object for individual participant recording.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-stage.html#cfn-ivs-stage-autoparticipantrecordingconfiguration
     */
    readonly autoParticipantRecordingConfiguration?: CfnStage.AutoParticipantRecordingConfigurationProperty | cdk.IResolvable;
    /**
     * Stage name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-stage.html#cfn-ivs-stage-name
     */
    readonly name?: string;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-stage-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-stage.html#cfn-ivs-stage-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a Stage resource.
 *
 * @struct
 * @stability external
 */
export interface StageReference {
    /**
     * The Arn of the Stage resource.
     */
    readonly stageArn: string;
}
/**
 * Indicates that this resource can be referenced as a StorageConfiguration.
 *
 * @stability experimental
 */
export interface IStorageConfigurationRef extends constructs.IConstruct {
    /**
     * A reference to a StorageConfiguration resource.
     */
    readonly storageConfigurationRef: StorageConfigurationReference;
}
/**
 * The `AWS::IVS::StorageConfiguration` resource specifies an  storage configuration.
 *
 * A storage configuration describes an S3 location where recorded videos will be stored. For more information, see [Auto-Record to Amazon S3 (Low-Latency Streaming)](https://docs.aws.amazon.com/ivs/latest/LowLatencyUserGuide/record-to-s3.html) in the *Amazon IVS Low-Latency Streaming User Guide* .
 *
 * @cloudformationResource AWS::IVS::StorageConfiguration
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-storageconfiguration.html
 */
export declare class CfnStorageConfiguration extends cdk.CfnResource implements cdk.IInspectable, IStorageConfigurationRef, cdk.ITaggableV2 {
    /**
     * The CloudFormation resource type name for this resource class.
     */
    static readonly CFN_RESOURCE_TYPE_NAME: string;
    /**
     * Build a CfnStorageConfiguration from CloudFormation properties
     *
     * A factory method that creates a new instance of this class from an object
     * containing the CloudFormation properties of this resource.
     * Used in the @aws-cdk/cloudformation-include module.
     *
     * @internal
     */
    static _fromCloudFormation(scope: constructs.Construct, id: string, resourceAttributes: any, options: cfn_parse.FromCloudFormationOptions): CfnStorageConfiguration;
    /**
     * The storage-configuration ARN. For example: `arn:aws:ivs:us-west-2:123456789012:storage-configuration/abcdABCDefgh`
     *
     * @cloudformationAttribute Arn
     */
    readonly attrArn: string;
    /**
     * Tag Manager which manages the tags for this resource
     */
    readonly cdkTagManager: cdk.TagManager;
    /**
     * Storage cnfiguration name.
     */
    name?: string;
    /**
     * An S3 storage configuration contains information about where recorded video will be stored.
     */
    s3: cdk.IResolvable | CfnStorageConfiguration.S3StorageConfigurationProperty;
    /**
     * An array of key-value pairs to apply to this resource.
     */
    tags?: Array<cdk.CfnTag>;
    /**
     * @param scope Scope in which this resource is defined
     * @param id Construct identifier for this resource (unique in its scope)
     * @param props Resource properties
     */
    constructor(scope: constructs.Construct, id: string, props: CfnStorageConfigurationProps);
    get storageConfigurationRef(): StorageConfigurationReference;
    protected get cfnProperties(): Record<string, any>;
    /**
     * Examines the CloudFormation resource and discloses attributes
     *
     * @param inspector tree inspector to collect and process attributes
     */
    inspect(inspector: cdk.TreeInspector): void;
    protected renderProperties(props: Record<string, any>): Record<string, any>;
}
export declare namespace CfnStorageConfiguration {
    /**
     * The S3StorageConfiguration property type describes an S3 location where recorded videos will be stored.
     *
     * @struct
     * @stability external
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-storageconfiguration-s3storageconfiguration.html
     */
    interface S3StorageConfigurationProperty {
        /**
         * Name of the S3 bucket where recorded video will be stored.
         *
         * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-storageconfiguration-s3storageconfiguration.html#cfn-ivs-storageconfiguration-s3storageconfiguration-bucketname
         */
        readonly bucketName: string;
    }
}
/**
 * Properties for defining a `CfnStorageConfiguration`
 *
 * @struct
 * @stability external
 * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-storageconfiguration.html
 */
export interface CfnStorageConfigurationProps {
    /**
     * Storage cnfiguration name.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-storageconfiguration.html#cfn-ivs-storageconfiguration-name
     */
    readonly name?: string;
    /**
     * An S3 storage configuration contains information about where recorded video will be stored.
     *
     * See the [S3StorageConfiguration](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-storageconfiguration-s3storageconfiguration.html) property type for more information.
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-storageconfiguration.html#cfn-ivs-storageconfiguration-s3
     */
    readonly s3: cdk.IResolvable | CfnStorageConfiguration.S3StorageConfigurationProperty;
    /**
     * An array of key-value pairs to apply to this resource.
     *
     * For more information, see [Tag](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ivs-storageconfiguration-tag.html) .
     *
     * @see http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ivs-storageconfiguration.html#cfn-ivs-storageconfiguration-tags
     */
    readonly tags?: Array<cdk.CfnTag>;
}
/**
 * A reference to a StorageConfiguration resource.
 *
 * @struct
 * @stability external
 */
export interface StorageConfigurationReference {
    /**
     * The Arn of the StorageConfiguration resource.
     */
    readonly storageConfigurationArn: string;
}
