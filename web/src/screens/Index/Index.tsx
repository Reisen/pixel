import React                    from 'react';
import NavigationBar            from '../../components/NavigationBar';
import SearchSidebar            from '../../components/SearchSideBar';
import ImageGrid                from './components/ImageGrid';
import styles                   from './Index.module.css';
import { image }                from '../../types/image';
import { connect }              from 'react-redux';
import { State }                from '../../store/reducers';
import { getImages }            from '../../store/reducers/images';

const tags: string[] = [
    "dog",
    "field",
    "happy_dog",
    "bernese"
];

interface Props {
    images: image[];
    page: number;
    pageWidth: number;
    pageCount: number;
    nextPage: () => void;
    prevPage: () => void;
}

const Index = (props: Props) => (
    <div className="Page">
        <NavigationBar />
        <div className={styles.PanelContainer}>
            <SearchSidebar tags={tags} />
            <ImageGrid images={props.images} rows={5} width={6} />
        </div>
    </div>
);

const mapState = (state: State) => ({
    images: getImages(state.images),
});

export default connect(mapState)(Index);
