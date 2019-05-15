import { Image as image } from '../../api/types';

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
