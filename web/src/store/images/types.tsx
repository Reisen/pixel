import { image }                              from '../../types/image';

export interface ImageState {
    images: image[];
}

export type State =
    | ImageState
    | undefined;
