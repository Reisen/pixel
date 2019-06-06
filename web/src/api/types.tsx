export type Token = IToken;

export interface IToken {
    _tokenText: string;
}

export type APIImage = IAPIImage;

export interface IAPIImage {
    _dimensions: [number, number];
    _filename: string;
    _filesize: number;
    _path: string;
    _tags: string[];
    _thumb: string;
    _uuid: string;
}

export type Request = IRequest;

export interface IRequest {
    _tags: string[];
}

export type Response = IResponse;

export interface IResponse {
    _images: APIImage[];
}

export type Response = IResponse;

export interface IResponse {
    _tags: string[];
}

export type Request = IRequest;

export interface IRequest {
    _path: string;
    _tags: string[];
}

export type Request = IRequest;

export interface IRequest {
    _tags: string[];
}

export type User = IUser;

export interface IUser {
    _uuid?: string;
    _username?: string;
    _email?: string;
    _role?: string;
}

export type Request = IRequest;

export interface IRequest {
    _email: string;
    _password: string;
}

export type Request = IRequest;

export interface IRequest {
    _email: string;
    _password: string;
}