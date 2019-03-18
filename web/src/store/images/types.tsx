import { image } from '../../types/image';

export interface ImageState {
    fillGrid: 'fill' | 'cover' | 'contain',
    images:   image[];
}

export type State =
    | ImageState
    | undefined;
