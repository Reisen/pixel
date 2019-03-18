import React, { useEffect }       from 'react';
import { image }                  from '../../types/image';
import { connect }                from 'react-redux';
import { match }                  from 'react-router-dom';
import { State }                  from '../../store';
import { getImages, fetchImages } from '../../store/images';
import { History }                from 'history'

import NavigationBar              from '../../components/NavigationBar';
import SearchSidebar              from '../../components/SearchSideBar';
import ImageGrid                  from './components/ImageGrid';
import styles                     from './Index.module.css';


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
    fetchImages: () => void;
    images:      image[];
    username:    string;
    history:     History;
    match:       match<Params>;
}

const Index = (props: Props) => {
    const pageNumber = parseInt(props.match.params.page || '1', 10);
    const changePage = (page: number) => {
        props.history.push(`/g/${page}`)
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
                    rows={4}
                    width={6}
                    page={pageNumber}
                    images={props.images}
                    changePage={changePage}
                />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    images: getImages(state.images),
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};

export default connect(mapState, mapDispatch)(Index);
