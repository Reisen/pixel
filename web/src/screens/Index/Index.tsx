import React         from 'react';
import NavigationBar from '../../components/NavigationBar';
import SearchSidebar from '../../components/SearchSideBar';
import ImageGrid     from './components/ImageGrid';
import styles        from './Index.module.css';
import { image }     from '../../types/image';
import { connect }   from 'react-redux';
import { State }     from '../../store/reducers';

const tags: string[] = [
    "dog",
    "field",
    "happy_dog",
    "bernese"
];

interface Props {
    images: image[]
}

const Index = (props: Props) => (
    <div className="Page">
        <NavigationBar />
        <div className={styles.PanelContainer}>
            <SearchSidebar tags={tags} />
            <ImageGrid width={6} images={props.images} />
        </div>
    </div>
);

const mapState = (state: State) => ({
    images: state.images.images
});

export default connect(mapState)(Index);
