export type Token = IToken;

export interface IToken {
    tokenText: string;
}

export type APIImage = IAPIImage;

export interface IAPIImage {
    dimensions: [number, number];
    filename: string;
    filesize: number;
    path: string;
    tags: string[];
    thumb: string;
    uuid: string;
}

export type Request = IRequest;

export interface IRequest {
    tags: string[];
}

export type Response = IResponse;

export interface IResponse {
    images: APIImage[];
}

export type Response = IResponse;

export interface IResponse {
    tags: string[];
}

export type Request = IRequest;

export interface IRequest {
    path: string;
    tags: string[];
}

export type Request = IRequest;

export interface IRequest {
    tags: string[];
}

export type User = IUser;

export interface IUser {
    uuid?: string;
    username?: string;
    email?: string;
    role?: string;
}

export type Request = IRequest;

export interface IRequest {
    email: string;
    password: string;
}

export type Request = IRequest;

export interface IRequest {
    email: string;
    password: string;
}