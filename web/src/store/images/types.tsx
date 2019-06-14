import { image } from '../../types/image';

export type ScalingMode =
    | 'fill'
    | 'cover'
    | 'contain';

export interface ImageState {
    galleryScalingMode: ScalingMode,
    images:             image[];
}

export type State =
    | ImageState
    | undefined;
