import { ImageState } from './types';

export const getImages = (state: ImageState) =>
    state.images;

export const getGalleryScaling = (state: ImageState) =>
    state.galleryScalingMode;
