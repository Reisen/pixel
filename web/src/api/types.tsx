export type Token = IToken;

export interface IToken {
    text: string;
}

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

export type User = IUser;

export interface IUser {
    username?: string;
    email?: string;
    role?: string;
}

export type AuthUserRequest = IAuthUserRequest;

export interface IAuthUserRequest {
    email: string;
    password: string;
}

export type RegisterRequest = IRegisterRequest;

export interface IRegisterRequest {
    email: string;
    password: string;
}