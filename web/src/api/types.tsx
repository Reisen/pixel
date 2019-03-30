export type Image = IImage;

export interface IImage {
    path: string;
    thumb: string;
    tags: string[];
    UUID: string;
}

export type DeleteTagsRequest = IDeleteTagsRequest;

export interface IDeleteTagsRequest {
    tags: string[];
}

export type GetImageResponse = IGetImageResponse;

export interface IGetImageResponse {
    images: Image[];
}

export type GetTagsResponse = IGetTagsResponse;

export interface IGetTagsResponse {
    tags: string[];
}

export type PostImageRequest = IPostImageRequest;

export interface IPostImageRequest {
    path: string;
    tags: string[];
}

export type PostTagsRequest = IPostTagsRequest;

export interface IPostTagsRequest {
    tags: string[];
}