import React, { useEffect }       from 'react';
import NavigationBar              from '../../components/NavigationBar';
import SearchSidebar              from '../../components/SearchSideBar';
import ImageGrid                  from './components/ImageGrid';
import styles                     from './Index.module.css';
import { image }                  from '../../types/image';
import { connect }                from 'react-redux';
import { State }                  from '../../store';
import { getImages, fetchImages } from '../../store/images';

const tags: string[] = [
    "dog",
    "field",
    "happy_dog",
    "bernese"
];

interface Props {
    fetchImages: () => void;
    images:      image[];
    nextPage:    () => void;
    page:        number;
    pageCount:   number;
    pageWidth:   number;
    prevPage:    () => void;
    username:    string;
}

const Index = (props: Props) => {
    useEffect(() => { props.fetchImages(); });

    return (
        <div className="Page">
            <NavigationBar username={props.username} />
            <div className={styles.PanelContainer}>
                <SearchSidebar tags={tags} />
                <ImageGrid images={props.images} rows={5} width={6} />
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
