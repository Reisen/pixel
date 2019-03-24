import React, { useEffect } from 'react';
import { image }            from '../../types/image';
import { connect }          from 'react-redux';
import { match }            from 'react-router-dom';
import { State }            from '../../store';
import { ScalingMode }      from '../../store/images/types';
import { History }          from 'history'
import {
    getImages,
    fetchImages,
    setGalleryScaling,
    getGalleryScaling
}                           from '../../store/images';

import NavigationBar        from '../../components/NavigationBar';
import SearchSidebar        from '../../components/SearchSideBar';
import ImageGrid            from './components/ImageGrid';
import styles               from './Index.module.css';


const tags: string[] = [
    "dog",
    "field",
    "happy_dog",
    "bernese"
];

interface Params {
    page?: string;
}

interface Props {
    fetchImages:        () => void;
    galleryScalingMode: ScalingMode;
    images:             image[];
    username:           string;
    setGalleryScaling:  (mode: ScalingMode) => void;
    history:            History;
    match:              match<Params>;
}

const Index = (props: Props) => {
    const pageNumber = parseInt(props.match.params.page || '1', 10);
    const changePage = (page: number) => {
        props.history.push(`/my/images/${page}`)
    };

    // Load Images, On First Mount Only
    useEffect(() => {
        props.fetchImages()
    }, []);

    return (
        <div className="Page">
            <NavigationBar username={props.username} />
            <div className={styles.PanelContainer}>
                <SearchSidebar initialPanel="tags" tags={tags} />
                <ImageGrid
                    changePage={changePage}
                    scalingMode={props.galleryScalingMode}
                    setScalingMode={props.setGalleryScaling}
                    images={props.images}
                    page={pageNumber}
                    rows={4}
                    width={6}
                />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    images:             getImages(state.images),
    galleryScalingMode: getGalleryScaling(state.images),
    username:           state.user.username
});

const mapDispatch = {
    fetchImages,
    setGalleryScaling
};

export default connect(mapState, mapDispatch)(Index);
